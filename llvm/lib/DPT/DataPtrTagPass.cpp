#include "llvm/DPT/DataPtrTagPass.h"
#include "llvm/DPT/DPT.h"

#define ASSERT(x) if (!(x)) *(voidptr) = 0;

char DataPtrTagPass::ID = 0;
static RegisterPass<DataPtrTagPass> X("dpt-tag", "Data-pointer tagging pass");

Pass *llvm::DPT::createDataPtrTagPass() { return new DataPtrTagPass(); }

bool DataPtrTagPass::runOnModule(Module &M) {
	init(M);

  if (BASE) {
		insertDptSet(M);
	  return false;
	}

	ASSERT(DPT_H || DPT_C || DPT_F);

	//handleIntrinsicFunctions(M);

	if (!DPT_H) {
  	handleGlobalVariables(M);
  	handleAllocaInsts(M);
	}

  //handleMallocs(M);

	if (DPT_F)
		handleGEPs(M);

	//// For Juliet test, need to comment out
  auto const dptXtagMode = llvm::DPT::getDptXtagMode();
  if (dptXtagMode == DptXtag::XtagEnable)
	  handlePtrToInts(M);

	insertDptSet(M);

	//if (QEMU)
	//  printFuncAddr(M);

  //handleWrapperFunctions(M);

	//if (QEMU)
	//	handleQEMU(M);

	errs() << "statNumGV: " << statNumGV << "\n";
	errs() << "statNumAI: " << statNumAI << "\n";
	errs() << "statNumCI: " << statNumCI << "\n";
	errs() << "statNumGEP: " << statNumGEP << "\n";

	return true; // function_modified = true
}

void DataPtrTagPass::handleGlobalVariables(Module &M) {
	set<GlobalVariable *> gv_set;

	for (auto &G : M.getGlobalList()) {
		GlobalVariable *pGV = dyn_cast<GlobalVariable>(&G);
		Type *ty = pGV->getValueType();

		// Skip ".str" constants
		if ((pGV->isConstant() && (pGV->getName().find(".str") == 0 || pGV->getName().find("str") == 0)) ||
				//(pGV->use_empty() || pGV->getLinkage() == GlobalValue::LinkOnceODRLinkage) || //TODO
				pGV->use_empty() || !pGV->hasInitializer() || pGV->isDeclaration() ||
        (!ty->isArrayTy() && !IsStructTyWithArray(ty)))
			continue;

    if (pGV->getName().find("reltable.") == 0) // do not handle relative tables
      continue;
    //errs() << "pGV: " << pGV->getName() << "\n";

		statNumGV++;
		gv_set.insert(pGV);
  }

	for (auto pGV: gv_set) {
		handleGlobalVariable(M, pGV);
		gvop_set.clear();
	}
}

void DataPtrTagPass::handleGlobalVariable(Module &M, GlobalVariable *pGV) {
	set<Operator*> op_set;
	gvop_set.insert(pGV);

	for (auto pU: pGV->users()) {
		if (dyn_cast<Instruction>(pU)) {
		} else if (auto pOp = dyn_cast<Operator>(pU)) {
			if (gvop_set.find(pOp) != gvop_set.end()) {
				errs() << "Should't be reached!\n";
				ASSERT(false);
			}

			gvop_set.insert(pOp);
		}
	}

	for (auto pOp: op_set) {
		findOperators(M, pOp);
	}

	map<Function*,set<Instruction*>*> func_map;

	for (auto pV: gvop_set) {
		for (auto pU: pV->users()) {
			if (auto pI = dyn_cast<Instruction>(pU)) {
				auto pF = pI->getFunction();

				if (!func_map[pF])
					func_map[pF] = new set<Instruction*>;

				func_map[pF]->insert(pI);
			}
		}
	}

	for (auto x: func_map) {
		auto pF = x.first;
		auto inst_set = x.second;

		IRBuilder<> Builder(&(pF->front().front()));
		//auto tagd = insertTagd(M, &Builder, pGV);
		auto tagd = insertTagd(M, &Builder, pGV, pGV);
		user_map[pGV] = tagd;

		for (auto pI: *inst_set) {
			unsigned cnt = 0;
			//errs() << "pI: "; pI->dump();
			for (auto itr = pI->op_begin(); itr != pI->op_end(); itr++, cnt++) {
				if (gvop_set.find(*itr) != gvop_set.end()) {
					auto new_val = user_map[*itr];

					if (!new_val)
						new_val = createOperator(M, pGV, *itr, tagd);

					pI->setOperand(cnt, new_val);
				}
			}
		}

		user_map.clear();
	}

	// In entry, store cap of gvs
	// There can be global variables that have the same memory address
	// Those can duplicate same metadata
	// For example, _ZN11xercesc_2_7L11fgValueZeroE, _ZN11xercesc_2_7L11fgValueZeroE.2892 in xalancbmk_r
	// To avoid duplicates, need to check
	Type *ty = pGV->getValueType();
	auto size = DL->getTypeSizeInBits(ty);
	Value *arg = ConstantInt::get(Type::getInt64Ty(*C), size / 8);

	IRBuilder<> Builder(&(entry->back().back()));
	//auto callA = insertTagd(M, &Builder, pGV);
	auto callA = insertTagd(M, &Builder, pGV, pGV);
	//insertCstr(M, &Builder, callA, arg);
	// Insert cstr
	auto callB = insertBextm(M, &Builder, callA);
	auto callC = insertCstr(M, &Builder, callA, arg);
	auto callD = insertBsetm(M, &Builder, callA);

	auto curBB = callA->getParent();
	auto newBB = curBB->splitBasicBlock(callC);
	auto newBB2 = newBB->splitBasicBlock(callD->getNextNode());
	auto pBI = &(curBB->back());
	IRBuilder<> BuilderB(pBI);
	BuilderB.CreateCondBr(callB, newBB2, newBB);
	pBI->eraseFromParent();
	return;
}

void DataPtrTagPass::findOperators(Module &M, Value *pV) {
	set<Operator*> op_set;

	for (auto pU: pV->users()) {
		if (dyn_cast<Instruction>(pU)) {
		} else if (auto pOp = dyn_cast<Operator>(pU)) {
			gvop_set.insert(pOp);
		}
	}

	for (auto pOp: op_set) {
		findOperators(M, pOp);
	}
}

Value *DataPtrTagPass::createOperator(Module &M, GlobalVariable *pGV, Value *pOp, Instruction *pTI) {
	Value *op0 = nullptr;
	Value *op1 = nullptr;
	Value *pI0 = nullptr;
	Value *pI1 = nullptr;

	auto op = dyn_cast<Operator>(pOp);
	switch (op->getOpcode()) {
		case Instruction::GetElementPtr:
		case Instruction::BitCast:
		case Instruction::PtrToInt:
		case Instruction::IntToPtr:
		{
			op0 = op->getOperand(0);

			if (op0 == pGV) {
				pI0 = pTI;
			} else {
				pI0 = user_map[op0];

				if (!pI0) {
					pI0 = createOperator(M, pGV, op0, pTI);
					user_map[op0] = pI0;
				}
			}

			IRBuilder<> Builder(dyn_cast<Instruction>(pI0)->getNextNode());

			Value *pI = nullptr;

			if (auto pGEP = dyn_cast<GEPOperator>(op)) {
				vector<Value *> indices;
				for (auto idx = pGEP->idx_begin(); idx != pGEP->idx_end(); idx++)
					indices.push_back(*idx);
				pI = Builder.CreateGEP(pGEP->getSourceElementType(), pI0, indices);
			} else if (auto pBC = dyn_cast<BitCastOperator>(op)) {
				pI = Builder.CreateCast(Instruction::BitCast, pI0, pBC->getType());
			} else if (auto pPTI = dyn_cast<PtrToIntOperator>(op)) {
				pI = Builder.CreatePtrToInt(pI0, pPTI->getType());
			} else if (op->getOpcode() == Instruction::IntToPtr) {
				pI = Builder.CreateIntToPtr(pI0, op->getType());
			}

			if (!pI) {
				errs() << "pI is still nullptr!\n";
				errs() << "op: "; op->dump();
				ASSERT(false);
			}

			return pI;

			break;
		}
		case Instruction::Add:
		case Instruction::Sub:
		case Instruction::And:
		{
			op0 = op->getOperand(0);
			op1 = op->getOperand(1);

			if (gvop_set.find(op0) != gvop_set.end()) {
				pI0 = user_map[op0];
			
				if (!pI0) {
					pI0 = createOperator(M, pGV, op0, pTI);
					user_map[op0] = pI0;
				}
			} else {
				pI0 = op0;
			}

			if (gvop_set.find(op1) != gvop_set.end()) {
				pI1 = user_map[op1];
			
				if (!pI1) {
					pI1 = createOperator(M, pGV, op1, pTI);
					user_map[op1] = pI1;
				}
			} else {
				pI1 = op1;
			}

			Value *pI = nullptr;
			if (dyn_cast<Instruction>(pI0)) {
				pI = pI0;
			}

			if (dyn_cast<Instruction>(pI1)) {
				if (pI && dyn_cast<Instruction>(pI1)->comesBefore(dyn_cast<Instruction>(pI0)))
					pI = pI0;
				else
					pI = pI1;
			}

			if (!pI) {
				errs() << "pI is nullptr!\n";
				errs() << "op: "; op->dump();
				ASSERT(false);
			}

			IRBuilder<> Builder(dyn_cast<Instruction>(pI)->getNextNode());

			if (op->getOpcode() == Instruction::Add) {
				auto add = Builder.CreateAdd(pI0, pI1);
				return add;
			} else if (op->getOpcode() == Instruction::Sub) {
				auto add = Builder.CreateSub(pI0, pI1);
				return add;
			} else if (op->getOpcode() == Instruction::And) {
				auto add = Builder.CreateAnd(pI0, pI1);
				return add;
			}
			break;
		}
		default:
		{
			errs() << "Operator not handled found!\n";
			errs() << "op: "; op->dump();
			ASSERT(false);
			break;
		}
	}
}

void DataPtrTagPass::handleAllocaInsts(Module &M) {
	set<AllocaInst *> alloca_set;

	for (auto &F : M) {
		for (auto &BB : F) {
			if (!BB.isEntryBlock())
				continue;
			for (auto &I : BB) {
				if (auto pAI = dyn_cast<AllocaInst>(&I)) {
					Type *ty = pAI->getAllocatedType();
					Function *pF = pAI->getFunction();

					if (ty->isArrayTy() || IsStructTyWithArray(ty)) {
						alloca_set.insert(pAI);
						statNumAI++;
					}
					//} else if (pAI->isArrayAllocation()) {
					//	// To handle void *alloca(size_t size); 
					//	// Found in Juliet test suite
					//	// This is not in POSIX...
					//	alloca_set.insert(pAI);
					//	statNumAI++;
					//	//errs() << "pAI->dump(): "; pAI->dump();
					//  //auto size = pAI->getAllocationSizeInBits(*DL);
					//	//errs() << "size: " << size.getValue() << "\n";
				}
			}
		}
	}

	for (auto pAI: alloca_set) {
		handleAlloca(M, pAI);
	}
}

void DataPtrTagPass::handleAlloca(Module &M, AllocaInst *pAI) {
	Type *ty = pAI->getAllocatedType();
  auto size = pAI->getAllocationSizeInBits(*DL);
	Value *arg = ConstantInt::get(Type::getInt64Ty(*C), (*size) / 8);
	//TODO if (size == llvm::None)
	//TODO 	return;

	// Find Return & Resume Inst
	auto pF = pAI->getFunction();
	Instruction *ret = nullptr;
	Instruction *resume = nullptr;

	for (auto &BB: *pF) {
		for (auto &I: BB) {
			if (dyn_cast<ReturnInst>(&I)) {
				ret = &I;
			} else if (dyn_cast<ResumeInst>(&I)) {
				resume = &I;
			}
		}
	}

	if (!ret && !resume) {
		errs() << "Couldn't find both ReturnInst and ResumeInst! " << pF->getName() << "\n";
		return;
	}

	//if (!IsStructTyWithArray(ty)) {
  IRBuilder<> Builder(pAI->getNextNode());

  // Insert tagd / cstr
  FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
  Value *num = ConstantInt::get(Type::getInt64Ty(*C), tagd_cnt++);
  auto tagd = QEMU? M.getOrInsertFunction("__tagd_num", FuncTypeA) :
                    Intrinsic::getDeclaration(&M, Intrinsic::pa_tagd, {Type::getInt8PtrTy(*C)});
  auto callA = QEMU? Builder.CreateCall(tagd, {pAI, pAI, num}, "") :
                    Builder.CreateCall(tagd, {pAI, pAI}, "");
  pAI->replaceAllUsesWith(callA);
  callA->setOperand(0, pAI);
  callA->setOperand(1, pAI);
  insertCstr(M, &Builder, callA, arg);

  if (ret) {
    IRBuilder<> Builder(ret);
    insertCclr(M, &Builder, callA);
  }

  if (resume) {
    IRBuilder<> Builder(resume);
    insertCclr(M, &Builder, callA);
  }
	//}

	if (DPT_F && IsStructTyWithArray(ty)) {
		//FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
		//auto scan = M.getOrInsertFunction("__scan_bitmap", FuncTypeA);

		if (ret) {
			//IRBuilder<> Builder(ret);
			//Builder.CreateCall(scan, {pAI, arg});
      insertCclrChains(M, pAI, dyn_cast<Instruction>(callA), ret);
		}

		if (resume) {
			//IRBuilder<> Builder(resume);
			//Builder.CreateCall(scan, {pAI, arg});
      insertCclrChains(M, pAI, dyn_cast<Instruction>(callA), resume);
		}
	}
}

void DataPtrTagPass::insertCclrChains(Module &M, AllocaInst *pAI, Instruction *pTI, Instruction *pRI) {
	Type *ty = pAI->getAllocatedType();
	vector<Value*> *indices = new vector<Value*>;
  //errs() << "pAI: "; pAI->dump();
  //errs() << "(1) ty: "; ty->dump();

  if (auto sty = dyn_cast<StructType>(ty)) {
  	indices->push_back(ConstantInt::get(Type::getInt32Ty(*C), 0));
    handleStructType(M, pAI, pTI, pRI, sty, indices);
  } else if (auto aty = dyn_cast<ArrayType>(ty)) {
    if (IsStructTyWithArray(aty)) { // struct array
      indices->push_back(ConstantInt::get(Type::getInt32Ty(*C), 0));
      handleStructArrayType(M, pAI, pTI, pRI, aty, indices);
    } else {
      indices->push_back(ConstantInt::get(Type::getInt32Ty(*C), 0));
      handleArrayType(M, pAI, pTI, pRI, aty, indices);
    }
  }

  delete indices;
}

void DataPtrTagPass::handleStructType(Module &M, AllocaInst *pAI, Instruction *pTI, Instruction *pRI, StructType *sty, vector<Value*> *indices) {
  //errs() << "(2) sty: "; sty->dump();
  unsigned n = sty->getNumElements();

  for (unsigned i=0; i<n; i++) {
    auto ety = sty->getElementType(i);
    vector<Value*> *indices_new = new vector<Value*>;

    // Copy indices
    for (auto pV: *indices)
      indices_new->push_back(pV);

    if (auto aty = dyn_cast<ArrayType>(ety)) {
      if (IsStructTyWithArray(aty)) { // struct array
      	indices_new->push_back(ConstantInt::get(Type::getInt32Ty(*C), i));
        handleStructArrayType(M, pAI, pTI, pRI, aty, indices_new);
      } else { // array of other type
      	indices_new->push_back(ConstantInt::get(Type::getInt32Ty(*C), i));
        handleArrayType(M, pAI, pTI, pRI, aty, indices_new);
      }
    } else if (IsStructTyWithArray(ety)) { // struct
      indices_new->push_back(ConstantInt::get(Type::getInt32Ty(*C), i));
      handleStructType(M, pAI, pTI, pRI, dyn_cast<StructType>(ety), indices_new);
    }

    delete indices_new;
  }
}

// %struct.Organisation = type { [20 x i8], [20 x i8], %struct.Employee }

void DataPtrTagPass::handleStructArrayType(Module &M, AllocaInst *pAI, Instruction *pTI, Instruction *pRI, ArrayType *aty, vector<Value*> *indices) {
  //errs() << "(3) aty: "; aty->dump();
  unsigned n = aty->getNumElements();

  for (unsigned i=0; i<n; i++) {
    auto ety = aty->getElementType();

    vector<Value*> *indices_new = new vector<Value*>;

    // Copy indices
    for (auto pV: *indices)
      indices_new->push_back(pV);

    if (ety->isArrayTy()) {
      indices_new->push_back(ConstantInt::get(Type::getInt32Ty(*C), i));
      handleStructArrayType(M, pAI, pTI, pRI, dyn_cast<ArrayType>(ety), indices_new);
    } else {
      indices_new->push_back(ConstantInt::get(Type::getInt32Ty(*C), i));
      handleStructType(M, pAI, pTI, pRI, dyn_cast<StructType>(ety), indices_new);
    }

    delete indices_new;
  }
}

void DataPtrTagPass::handleArrayType(Module &M, AllocaInst *pAI, Instruction *pTI, Instruction *pRI, ArrayType *aty, vector<Value*> *indices) {
  auto zero = ConstantPointerNull::get(PointerType::get(Type::getInt8Ty(*C), 0));
  IRBuilder<> Builder(pRI);

  //errs() << "(4) aty: "; aty->dump();
	auto pGEP = Builder.CreateGEP(pAI->getAllocatedType(), pAI, *indices);
  //errs() << "pGEP: "; pGEP->dump();
	auto callF = insertBextm(M, &Builder, pGEP);
	//auto callA = insertTagd(M, &Builder, pGEP);
	auto callA = insertTagd(M, &Builder, pGEP, zero);
	auto callC = insertCclr(M, &Builder, callA);
	auto callD = insertBclrm(M, &Builder, callA);

	auto curBB = dyn_cast<Instruction>(pGEP)->getParent();
	auto newBB = curBB->splitBasicBlock(callA);
	auto newBB2 = newBB->splitBasicBlock(callD->getNextNode());
	auto pBI = &(curBB->back());
	IRBuilder<> BuilderB(pBI);
	BuilderB.CreateCondBr(callF, newBB, newBB2);
	pBI->eraseFromParent();

  // To prevent this pGEP from being handled by handleGEP()
	gep_visit_set.insert(dyn_cast<GetElementPtrInst>(pGEP));
}

void DataPtrTagPass::handleMallocs(Module &M) {
	// Create wrapper functions
	for (auto &F: M) {
		if (F.getName() == "malloc" ||
				F.getName() == "_Znwm" ||
				F.getName() == "_ZnwmRKSt9nothrow_t" ||
				F.getName() == "_Znam" ||
				F.getName() == "calloc" ||
				F.getName() == "realloc") {
			Function *wrap = nullptr;
			set<Value*> func_users;

			for (auto pU: F.users())
				func_users.insert(pU);

			for (auto pU: func_users) {
				if (auto pI = dyn_cast<Instruction>(pU)) {
					bool is_called_func = false;

					// Only handle CallInst
					// because InvokeInst may have PHINode Inst at normal dest
					if (auto pCI = dyn_cast<CallInst>(pI)) {
						if (&F == pCI->getCalledFunction()) {
							is_called_func = true;
							handleMalloc(M, pCI);
							statNumCI++;
						}
					}

					if (!is_called_func) {
						if (!wrap)
							wrap = createWrapperFunction(M, F, false);

						for (unsigned i=0; i<pI->getNumOperands(); i++) {
							if (pI->getOperand(i) == &F) {
								pI->setOperand(i, wrap);
							}
						}
					}
				}
			}
		}
	}
}

void DataPtrTagPass::handleMalloc(Module &M, CallBase *pCB) {
	IRBuilder<> Builder(pCB->getNextNode());
	//auto callA = insertTagd(M, &Builder, pCB);
	auto callA = insertTagd(M, &Builder, pCB, pCB);
	pCB->replaceAllUsesWith(callA);
	callA->setOperand(0, pCB);
	callA->setOperand(1, pCB);
}

void DataPtrTagPass::handleGEPs(Module &M) {
	list <GetElementPtrInst *> gep_list;

	for (auto &F : M) {
		if (F.getSection().find(".text.startup") != std::string::npos)
			continue;

		for (auto &BB: F) {
			for (auto &I: BB) {
				if (auto pGEP = dyn_cast<GetElementPtrInst>(&I)) {
					// If ResultElementType is array type
					if (pGEP->getResultElementType()->isArrayTy()) {
						// Do not narrow array-to-array GEPs
						// [TODO] Do not narrow struct array-to-struct GEPs
						if (pGEP->getSourceElementType()->isArrayTy())
							continue;

						//if (pGEP->getResultElementType()->getArrayElementType()->isArrayTy())
						//	continue;

						//if (pGEP->getResultElementType()->getArrayElementType()->isStructTy()) {
            //  if (pGEP->getResultElementType()->getArrayNumElements() > 1) {
            //    errs() << "Found struct array!\n";
            //    errs() << "-- Src: "; pGEP->getSourceElementType()->dump();
            //    errs() << "-- Res: "; pGEP->getResultElementType()->dump();
            //  }
						//	//continue;
            //}

            // 
            if (pGEP->getResultElementType()->getArrayNumElements() <= 1) {
              //errs() << "Ty: "; pGEP->getResultElementType()->dump();
              continue;
            }

						gep_list.push_back(pGEP);
					}
				}
			}
		}
	}

	for (auto pGEP: gep_list)
		handleGEP(M, pGEP);
	for (auto pGEP: gep_erase_set)
		pGEP->eraseFromParent();
}

void DataPtrTagPass::handleGEP(Module &M, GetElementPtrInst *pGEP) {
	// Skip if numIndices == 1 && index == 0
	if (pGEP->getNumIndices() == 1) {
		ConstantInt *idx = dyn_cast<ConstantInt>(*(pGEP->idx_begin()));

		if (idx && idx->getSExtValue() == 0)
			return;
	}

	auto size = DL->getTypeSizeInBits(pGEP->getResultElementType());

	// Workaround the issue with custom data structure (e.g., arr[0])
	if (size == 0)
		return;

	// Check if this GEP has already been handled
	if (gep_visit_set.find(pGEP) != gep_visit_set.end())
		return;

	Value *arg = ConstantInt::get(Type::getInt64Ty(*C), size / 8);
	auto base = pGEP->getPointerOperand();

	list <GetElementPtrInst *> gep_list;
	gep_list.push_back(pGEP);
	gep_visit_set.insert(pGEP);

	//// Find GEPs that are accessed from the same base with the same indices
	//// Replace all such GEPs with "a" mutated pointer
	////errs() << "base->dump(): "; base->dump();
	//for (auto pU: base->users()) {
	//	if (pU == pGEP)
	//		continue;

	//	if (auto gep_user = dyn_cast<GetElementPtrInst>(pU)) {
	//		// If # indices are different, no need to compare indices
	//		if (pGEP->getNumIndices() != gep_user->getNumIndices())
	//			continue;

	//		// If not in the same BB, shouldn't touch
	//		if (pGEP->getParent() != gep_user->getParent())
	//			continue;

	//		int i = 0;
	//		for (i=0; i<(pGEP->getNumIndices() + 1); i++) {
	//			if (pGEP->getOperand(i) != gep_user->getOperand(i))
	//				break;
	//		}

	//		if (i == (pGEP->getNumIndices() + 1)) {
	//			//errs() << "gep_user->dump(): "; gep_user->dump();
	//			gep_list.push_back(gep_user);
	//			gep_visit_set.insert(gep_user);
	//		}
	//	}	
	//}

	IRBuilder<> Builder(pGEP->getNextNode());
  auto zero = ConstantPointerNull::get(PointerType::get(Type::getInt8Ty(*C), 0));
	//auto callE = insertXtag(M, &Builder, pGEP); // TODO workaround hw issue for now
	//auto callA = insertTagd(M, &Builder, callE); // TODO workaround hw issue for now
	//auto callA = insertTagd(M, &Builder, pGEP);
	auto callA = insertTagd(M, &Builder, pGEP, zero);
	auto callB = insertBextm(M, &Builder, callA);
	auto callC = insertCstr(M, &Builder, callA, arg);
	auto callD = insertBsetm(M, &Builder, callA);

	pGEP->replaceAllUsesWith(callA);
	callA->setOperand(0, pGEP);
	//callA->setOperand(1, pGEP);
	//callE->setOperand(0, pGEP); // TODO workaround hw issue for now

	//for (auto gep: gep_list) {
	//	if (gep == pGEP)
	//		continue;

	//	gep->replaceAllUsesWith(castA);
	//	if (gep != pGEP)
	//		gep_erase_set.insert(gep);
	//}

	statNumGEP++;

	auto curBB = pGEP->getParent();
	auto newBB = curBB->splitBasicBlock(callC);
	auto newBB2 = newBB->splitBasicBlock(callD->getNextNode());

	auto pBI = &(curBB->back());
	IRBuilder<> BuilderB(pBI);
	BuilderB.CreateCondBr(callB, newBB2, newBB);
	pBI->eraseFromParent();
}

void DataPtrTagPass::handlePtrToInts(Module &M) {
	set<PtrToIntInst*> pti_set;

	for (auto &F : M) {
		for (auto &BB : F) {
			for (auto &I : BB) {
				//if (auto pPTI = dyn_cast<PtrToIntInst>(&I)) {
				//	pti_set.insert(pPTI);
				//} else if (auto pCI = dyn_cast<ICmpInst>(&I)) {
				if (auto pCI = dyn_cast<ICmpInst>(&I)) {
					//pCI->dump();
					ASSERT(pCI->getNumOperands() == 2);

					Value *val0 = pCI->getOperand(0);
					auto *const0 = dyn_cast<ConstantPointerNull>(val0);
					Type *ty0 = pCI->getOperand(0)->getType();

					Value *val1 = pCI->getOperand(1);
					auto *const1 = dyn_cast<ConstantPointerNull>(val1);
					Type *ty1 = pCI->getOperand(1)->getType();

					if (const0 || const1)
						continue;

					if (PointerType *pty = dyn_cast<PointerType>(ty0))
						addXtag(pCI, 0, M);

					if (PointerType *pty = dyn_cast<PointerType>(ty1))
						addXtag(pCI, 1, M);

          //if (!ty0->isPointerTy() || !ty1->isPointerTy())
          //  continue;

		      //IRBuilder<> Builder(pCI);
          //FunctionType *FuncTypeB = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
          //Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
          //auto chk_icmp = M.getOrInsertFunction("__check_icmp", FuncTypeB);
          //Builder.CreateCall(chk_icmp, {pCI->getOperand(0), pCI->getOperand(1), num}, "");
        }
			}
		}
	}

  return;

	for (auto pPTI: pti_set) {
		set<Instruction*> inst_set;

		for (auto pU: pPTI->users()) {
			// In perlbench_r, *p++ = PtrToInt(pointer to free);
			// In this case, the pointer is not used for pointer arithmetic / comparison
			// So, we shouldn't strip the tag
			if (auto pSI = dyn_cast<StoreInst>(pU))
				continue;

			// In C++ vector impl,
			// typecast to i64 before return
			if (auto pRI = dyn_cast<ReturnInst>(pU))
				continue;

			if (auto pI = dyn_cast<Instruction>(pU)) {
				inst_set.insert(pI);
			}
		}

		IRBuilder<> Builder(pPTI);
		auto callA = insertXtag(M, &Builder, pPTI->getOperand(0));
		auto callB = Builder.CreatePtrToInt(callA, pPTI->getType());

		for (auto pI: inst_set) {
			for (unsigned i=0; i<pI->getNumOperands(); i++) {
				if (pI->getOperand(i) == pPTI) {
					pI->setOperand(i, callB);
				}
			}
		}
	}
}

Instruction *DataPtrTagPass::insertTagd(Module &M, IRBuilder<> *Builder, Value *pV, Value *context) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
  Value *num = ConstantInt::get(Type::getInt64Ty(*C), tagd_cnt++);

	auto tagd = QEMU? M.getOrInsertFunction("__tagd_num", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::pa_tagd, {Type::getInt8PtrTy(*C)});
	auto callA = QEMU? Builder->CreateCall(tagd, {pV, context, num}, "") :
										Builder->CreateCall(tagd, {pV, context}, "");

	return callA;
}

Instruction *DataPtrTagPass::insertXtag(Module &M, IRBuilder<> *Builder, Value *pV) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
  Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);

	auto xtag = QEMU? M.getOrInsertFunction("__xtag_num", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::pa_xtag, {Type::getInt8PtrTy(*C)});
	auto callA = QEMU? Builder->CreateCall(xtag, {pV, num}, "") :
										Builder->CreateCall(xtag, {pV}, "");

	return callA;
}

Instruction *DataPtrTagPass::insertCstr(Module &M, IRBuilder<> *Builder, Value *pV, Value *arg) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);
  Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);

	auto cstr = QEMU? M.getOrInsertFunction("__cstr_num", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::dpt_cstr);
	auto callA = QEMU? Builder->CreateCall(cstr, {pV, arg, num}, "") :
										Builder->CreateCall(cstr, {pV, arg}, "");

	//if (!QEMU) {
	//	FunctionType *FuncTypeB = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);
	//	auto chk_cstr = M.getOrInsertFunction("__check_cstr", FuncTypeB);
	//	Builder->CreateCall(chk_cstr, {pV, arg, num}, "");
	//}

	return callA;
}

Instruction *DataPtrTagPass::insertCclr(Module &M, IRBuilder<> *Builder, Value *pV) {
	//FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C)}, false);
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
  Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);

	auto cclr = QEMU? M.getOrInsertFunction("__cclr_num", FuncTypeA) :
	//auto cclr = QEMU? M.getOrInsertFunction("__cclr", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::dpt_cclr);
	auto callA = QEMU? Builder->CreateCall(cclr, {pV, num}, "") :
										Builder->CreateCall(cclr, {pV}, "");
	//auto callA = Builder->CreateCall(cclr, {castA}, "");

	//if (!QEMU) {
	//	auto chk_cclr = M.getOrInsertFunction("__check_cclr", FuncTypeA);
	//	Builder->CreateCall(chk_cclr, {castA, num}, "");
	//}

	return callA;
}

Instruction *DataPtrTagPass::insertBsetm(Module &M, IRBuilder<> *Builder, Value *pV) {
  //FunctionType *FuncTypeB = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
  //Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
  //auto chk_cstr = M.getOrInsertFunction("__check_bsetm", FuncTypeB);
  //Builder->CreateCall(chk_cstr, {pV, num}, "");

	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C)}, false);

	auto bsetm = QEMU? M.getOrInsertFunction("__bsetm", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::bm_bsetm);
	auto callA = Builder->CreateCall(bsetm, {pV}, "");

	return callA;
}

Instruction *DataPtrTagPass::insertBclrm(Module &M, IRBuilder<> *Builder, Value *pV) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C)}, false);

	auto bclrm = QEMU? M.getOrInsertFunction("__bclrm", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::bm_bclrm);
	//auto callA = Builder->CreateCall(bclrm, {castA}, "");
	auto callA = Builder->CreateCall(bclrm, {pV}, "");

	return callA;
}

Instruction *DataPtrTagPass::insertBextm(Module &M, IRBuilder<> *Builder, Value *pV) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getInt64Ty(*C), {Type::getInt8PtrTy(*C)}, false);

	auto bextm = QEMU? M.getOrInsertFunction("__bextm", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::bm_bextm, {Type::getInt64Ty(*C)});
	//auto callA = Builder->CreateCall(bextm, {castA}, "");
	auto callA = Builder->CreateCall(bextm, {pV}, "");
	auto castB = dyn_cast<Instruction>(Builder->CreateTrunc(callA, Type::getInt1Ty(*C)));

		//FunctionType *FuncTypeB = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
	  //Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
		//auto chk_cstr = M.getOrInsertFunction("__check_bextm", FuncTypeB);
		//Builder->CreateCall(chk_cstr, {pV, num}, "");

	return castB;
}

void DataPtrTagPass::addXtag(Instruction *pI, unsigned idx, Module &M) {
  auto ptr = pI->getOperand(idx);

  IRBuilder<> Builder(pI);
	Value *num = ConstantInt::get(Type::getInt64Ty(*C), intr_num++);

  FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
  auto xtag = QEMU? M.getOrInsertFunction("__xtag_num", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::pa_xtag, {Type::getInt8PtrTy(*C)});
  auto callA = QEMU? Builder.CreateCall(xtag, {ptr, num}, "") :
										Builder.CreateCall(xtag, {ptr}, "");
  pI->setOperand(idx, callA);
}

bool DataPtrTagPass::IsStructTyWithArray(Type *ty) {
	while (ty->isArrayTy())
		ty = ty->getArrayElementType();

	if (auto str_ty = dyn_cast<StructType>(ty)) {
		for (auto it = str_ty->element_begin(); it != str_ty->element_end(); it++) {
			Type *ety = (*it);

			if (ety->isArrayTy())
				return true;
			if (ety->isStructTy() && IsStructTyWithArray(ety))
				return true;
		}
	}

	return false;
}

void DataPtrTagPass::init(Module &M) {
	Function *start = nullptr;

	for (auto &F : M) {
		if (&F && F.getSection().find(".text.startup") != std::string::npos) {
			start = &F;
			break;
		}
	}

	for (auto &F : M) {
		if (&F && F.getName() == "main") {
		  C = &F.getContext();
			DL = &F.getParent()->getDataLayout();
			main = &F;
			if (!start)
				start = main;
			break;
		}
	}

	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), false);
	entry = Function::Create(FuncTypeA, Function::ExternalLinkage, "__init_dpt", M);
	auto pBB = BasicBlock::Create(*C, "", entry, nullptr);
	ReturnInst::Create(*C, pBB);

	auto &BB = start->front();
	auto &I = BB.front();
	IRBuilder<> Builder(&I);		
	Builder.CreateCall(FuncTypeA, entry);

  auto const dptInstType = llvm::DPT::getDptInstType();
  auto const dptQemuMode = llvm::DPT::getDptQemuMode();

	BASE = (dptInstType == DptType::None);
	DPT_H = (dptInstType == DptType::DPT_H);
	DPT_C = (dptInstType == DptType::DPT_C);
	DPT_F = (dptInstType == DptType::DPT_F);
	QEMU = (dptQemuMode == DptQemu::Enable);

	errs() << "Start DPT pass!\n";
	errs() << "-- DPT_H: " << DPT_H << "\n";
	errs() << "-- DPT_C: " << DPT_C << "\n";
	errs() << "-- DPT_F: " << DPT_F << "\n";
	errs() << "-- QEMU : " << QEMU << "\n";
}

void DataPtrTagPass::handleIntrinsicFunctions(Module &M) {
  set <CallBase*> call_set;

	for (auto &F: M) {
		for (auto &BB: F) {
			for (auto &I: BB) {

				if (auto pCB = dyn_cast<CallBase>(&I)) {
					auto pF = pCB->getCalledFunction();

					if (!pF)
						continue;

          if (pF->getName() == "llvm.load.relative.i64") {
            //errs() << "Found load relative! "; pCB->dump();
            call_set.insert(pCB);
          }
        }
      }
    }
  }

  for (auto pCB: call_set) {
    IRBuilder<> Builder(pCB->getNextNode());
    auto callA = insertXtag(M, &Builder, pCB);
    pCB->replaceAllUsesWith(callA);
    callA->setOperand(0, pCB);
  }

  return;

	set<string> black_funcs({
		"fopen", "ftell", "fdopen", "fread", "ferror", "fwrite",
		"fseek", "fputc", "fputs", "ftime", "feof", "fgets",	"fclose",
		"opendir", "readdir", "closedir", "chdir", "mkdir", "rmdir", "stat", "stat64",
		"getenv", "putenv", "readdir64", "frexp", "execv", "execvp", "chmod", 
		"realpath", "getcwd", // "time", "localtime_r", "localtime", "asctime",
		"fopen64", "freopen64", "fileno", "open64", "read", "write", "fstat64", "fgetc",
		"ungetc", 
		"link", "unlink", "setvbuf", "clearerr", "gettimeofday",
		""
	});

	set<string> white_funcs({
		"malloc", "_Znwm", "_ZnwmRKSt9nothrow_t", "_Znam", "calloc", "realloc",
    "free", "_ZdlPv", "_ZdaPv",
		"__sigsetjmp", "_setjmp", "siglongjmp", "longjmp", "__xtag_num",
		"strlen", "strchr", "strtol", "strcmp", "strcpy", "strncpy",
		"strncmp", "strstr", "strspn", "strcspn", "strrchr", "strtoul",
		"strtod", "strcat", "strncat", "strcasecmp", "strncasecmp",
		"atoi", "atof", "atol", "memcmp", "__isoc99_scanf",
		"__gxx_personality_v0",
		"__cxa_begin_catch", "__cxa_throw", "__cxa_free_exception",
		"__cxa_guard_acquire", "__cxa_guard_release", "__cxa_guard_abort",
		"__cxa_atexit",
		//"fprintf", "fflush",
		"printf", "scanf", "sprintf"
	});

	for (auto &F: M) {
		for (auto &BB: F) {
			for (auto &I: BB) {
				if (auto pCB = dyn_cast<CallBase>(&I)) {
					auto pF = pCB->getCalledFunction();

					if (!pF)
						continue;

					bool chk = false;
					for (auto name: black_funcs) {
						if (pF->getName() == name) {
							chk = true;
							break;
						}
					}

					if (!chk)
						continue;

					errs() << "Handle: " << pF->getName() << "\n";

					IRBuilder<> Builder(pCB);
					unsigned idx = 0;
					for (auto arg = pCB->arg_begin(); arg != pCB->arg_end(); ++arg) {
						if (Value *pV = dyn_cast<Value>(arg)) {
							if (dyn_cast<ConstantPointerNull>(pV)) {
								idx++;
								continue;
							}

							if (PointerType *pty = dyn_cast<PointerType>(pV->getType())) {
								auto callA = insertXtag(M, &Builder, pCB->getOperand(idx));
								pCB->setOperand(idx, callA);
							}

							idx++;
						}
					}
				}
			}
		}
	}

	return;



	for (auto &F: M) {
		if (F.isDeclaration() && F.isVarArg()) {
			errs() << "VarArg: " << F.getName() << "\n";
		}

		if (!F.isDeclaration())
			continue;

		// Do not need to handle llvm intrinsics
		if (F.getName().find("llvm.") != string::npos)
			continue;

		// Check if function is a safe one
		bool white_chk = false;

		for (auto name: white_funcs) {
			if (F.getName() == name) {
				white_chk = true;
				break;
			}
		}

		if (white_chk)
			continue;

		// Check if pointer type operand exists
		auto fty = F.getFunctionType();
		bool argty_chk = false;

		for (unsigned i=0; i<fty->getNumParams(); i++) {
			if (fty->getParamType(i)->isPointerTy()) {
				argty_chk = true;
				break;
			}
		}

		if (!argty_chk)
			continue;

		Function *wrap = nullptr;
		set<Value*> func_users;

		for (auto pU: F.users())
			func_users.insert(pU);

		// I think if func is used as an operand, it should be forced too...
		bool wrap_force = false;
		for (auto pU: func_users) {
			if (dyn_cast<Instruction>(pU)) {
				//
			} else {
				wrap_force = true;
				break;
			}
		}

		if (wrap_force) {
			// Currently, VarArg function is not handled
			if (F.isVarArg()) {
				errs() << F.getName() << " may not be handled\n";
				continue;
			}

			errs() << "Force wrap: " << F.getName() << "\n";
			wrap = createWrapperFunction(M, F, true);

			continue;
		}

		// Check if function is a unsafe one
		bool black_chk = false;

		for (auto name: black_funcs) {
			if (F.getName() == name) {
				black_chk = true;
				break;
			}
		}

		if (!black_chk) {
			errs() << "Func Name: " << F.getName() << "\n";
			//continue;
		}
	
		// Check if function is ever used as a function pointer
		for (auto pU: func_users) {
			if (auto pI = dyn_cast<Instruction>(pU)) {
				if (pI->getFunction()->getSection().find(".text.startup") != std::string::npos)
					continue;

				bool is_called_func = false;
				if (auto pCB = dyn_cast<CallBase>(pU)) {
					if (&F == pCB->getCalledFunction())
						is_called_func = true;
				}

				if (is_called_func) {
					for (unsigned i=0; i<pI->getNumOperands()-1; i++) {
						auto pV = pI->getOperand(i);

						if (dyn_cast<ConstantPointerNull>(pV))
							continue;

						if (dyn_cast<PointerType>(pV->getType()))
							addXtag(pI, i, M);
					}
				} else {
					// Currently, VarArg function is not handled
					if (F.isVarArg()) {
						errs() << F.getName() << " may not be handled\n";
						continue;
					}

					if (!wrap)
						wrap = createWrapperFunction(M, F, false); 
					for (unsigned i=0; i<pI->getNumOperands(); i++) {
						if (pI->getOperand(i) == &F) {
							pI->setOperand(i, wrap);
						}
					}
				}
			}
		}
	}

	return;


//	set<string> white_funcs({
//		"malloc", "_Znwm", "_ZnwmRKSt9nothrow_t", "_Znam", "calloc", "realloc",
//    "free", "_ZdlPv", "_ZdaPv",
//		"__sigsetjmp", "_setjmp", "siglongjmp", "longjmp", "__xtag_num",
//		"strlen", "strchr", "strtol", "strcmp", "strcpy", "strncpy",
//		"strncmp", "strstr", "strspn", "strcspn", "strrchr", "strtoul",
//		"strtod", "strcat", "strncat", "strcasecmp", "strncasecmp",
//		"atoi", "atof", "atol", "memcmp", "__isoc99_scanf",
//		"__gxx_personality_v0",
//		"printf", "scanf", "sprintf"
//	});
//
//	for (auto &F: M) {
//		if (F.isDeclaration() && F.isVarArg()) {
//			errs() << "VarArg: " << F.getName() << "\n";
//		}
//
//		if (!F.isDeclaration())
//			continue;
//
//		// Do not need to handle llvm intrinsics
//		if (F.getName().find("llvm.") != string::npos)
//			continue;
//
//		// Check if function is a safe one
//		bool white_chk = false;
//
//		for (auto name: white_funcs) {
//			if (F.getName() == name) {
//				white_chk = true;
//				break;
//			}
//		}
//
//		if (white_chk)
//			continue;
//
//		// Check if pointer type operand exists
//		auto fty = F.getFunctionType();
//		bool argty_chk = false;
//
//		for (unsigned i=0; i<fty->getNumParams(); i++) {
//			if (fty->getParamType(i)->isPointerTy()) {
//				argty_chk = true;
//				break;
//			}
//		}
//
//		if (!argty_chk)
//			continue;
//
//		Function *wrap = nullptr;
//		set<Value*> func_users;
//
//		for (auto pU: F.users())
//			func_users.insert(pU);
//
//		//
//		bool wrap_force = false;
//		for (auto pU: func_users) {
//			if (dyn_cast<Instruction>(pU)) {
//				//
//			} else {
//				wrap_force = true;
//				break;
//			}
//		}
//
//		if (wrap_force) {
//			// Currently, VarArg function is not handled
//			if (F.isVarArg()) {
//				errs() << F.getName() << " may not be handled\n";
//				continue;
//			}
//
//			errs() << "Force wrap: " << F.getName() << "\n";
//			wrap = createWrapperFunction(M, F, true);
//
//			continue;
//		}
//	
//
//		// Check if function is ever used as a function pointer
//		for (auto pU: func_users) {
//			if (auto pI = dyn_cast<Instruction>(pU)) {
//				if (pI->getFunction()->getSection().find(".text.startup") != std::string::npos)
//					continue;
//
//				bool is_called_func = false;
//				if (auto pCB = dyn_cast<CallBase>(pU)) {
//					if (&F == pCB->getCalledFunction())
//						is_called_func = true;
//				}
//
//				if (is_called_func) {
//					for (unsigned i=0; i<pI->getNumOperands()-1; i++) {
//						auto pV = pI->getOperand(i);
//
//						if (dyn_cast<ConstantPointerNull>(pV))
//							continue;
//
//						if (dyn_cast<PointerType>(pV->getType()))
//							addXtag(pI, i, M);
//					}
//				} else {
//					// Currently, VarArg function is not handled
//					if (F.isVarArg()) {
//						errs() << F.getName() << " may not be handled\n";
//						continue;
//					}
//
//					if (!wrap)
//						wrap = createWrapperFunction(M, F, false); 
//					for (unsigned i=0; i<pI->getNumOperands(); i++) {
//						if (pI->getOperand(i) == &F) {
//							pI->setOperand(i, wrap);
//						}
//					}
//				}
//			}
//		}
//	}
//
//	// Handle intrinsic functions
//	for (auto &F: M) {
//    for (auto &BB: F) {
//      for (auto &I: BB) {
//				Function *pF = nullptr;
//
//        if (CallInst *pCI = dyn_cast<CallInst>(&I))
//          pF = pCI->getCalledFunction();
//        else if (InvokeInst *pII = dyn_cast<InvokeInst>(&I))
//          pF = pII->getCalledFunction();
//
//				if (pF)
//					continue;
//
//				if (auto *pCB = dyn_cast<CallBase>(&I)) {
//					unsigned idx = 0;
//
//					for (auto arg = pCB->arg_begin(); arg != pCB->arg_end(); ++arg) {
//						if (Value *pV = dyn_cast<Value>(arg)) {
//							if (dyn_cast<ConstantPointerNull>(pV)) {
//								idx++;
//								continue;
//							}
//
//							if (PointerType *pty = dyn_cast<PointerType>(pV->getType())) {
//								addXtag(pCB, idx, M);
//							}
//						}
//
//						idx++;
//					}
//				}
//			}
//		}
//	}
}

Function *DataPtrTagPass::createWrapperFunction(Module &M, Function &F, bool replace_use) {
	auto fty = F.getFunctionType();
	auto wrap = Function::Create(fty, Function::ExternalLinkage, "__dpt_" + F.getName(), M);
	auto pBB = BasicBlock::Create(*C, "", wrap, nullptr);

	if (replace_use)
		F.replaceAllUsesWith(wrap);

	vector<Value *> args;
	for (auto arg = wrap->arg_begin(); arg != wrap->arg_end(); arg++)
		args.push_back(arg);

	auto pCI = CallInst::Create(fty, &F, args, "", pBB);

	if (fty->getReturnType() == Type::getVoidTy(*C))
		ReturnInst::Create(*C, pBB);
	else
		ReturnInst::Create(*C, pCI, pBB);

	wrap->addFnAttr(Attribute::NoInline);
	wrap->addFnAttr(Attribute::NoUnwind);
	wrap->addFnAttr(Attribute::OptimizeNone);

	IRBuilder<> BuilderA(pCI);
	IRBuilder<> BuilderB(pCI->getNextNode());

	auto arg0 = pCI->getOperand(0);

	if (F.getName() == "malloc" ||
			F.getName() == "calloc" ||
			F.getName() == "realloc" ||
			F.getName() == "_Znwm" ||
			F.getName() == "_ZnwmRKSt9nothrow_t" ||
			F.getName() == "_Znam") {
		//auto callA = insertTagd(M, &BuilderB, pCI);
		auto callA = insertTagd(M, &BuilderB, pCI, pCI);
		pCI->replaceAllUsesWith(callA);
		callA->setOperand(0, pCI);
		callA->setOperand(1, pCI);
	} else {
		for (unsigned i=0; i<pCI->getNumOperands()-1; i++) {
			auto pV = pCI->getOperand(i);

			if (dyn_cast<ConstantPointerNull>(pV))
				continue;

			if (dyn_cast<PointerType>(pV->getType()))
				addXtag(pCI, i, M);
		}
	}

	return wrap;
}

void DataPtrTagPass::handleQEMU(Module &M) {
	for (auto &F: M) {
    for (auto &BB: F) {
      for (auto &I: BB) {
				bool found = false;
				Value *ptr = nullptr;

        if (LoadInst *pLI = dyn_cast<LoadInst>(&I)) {
					found = true;
					ptr = pLI->getOperand(0);
        } else if (StoreInst *pSI = dyn_cast<StoreInst>(&I)) {
					found = true;
					ptr = pSI->getOperand(1);
				}

				if (found) {
					IRBuilder<> Builder(&I);
					Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
					FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
					auto check = M.getOrInsertFunction("__check_num", FuncTypeA);
					Builder.CreateCall(check, {ptr, num}, "");
				}
			}
		}
	}
}

void DataPtrTagPass::insertDptSet(Module &M) {
  //for (auto &F : M) {
  //  if (&F && !F.isDeclaration()) {
  //    auto &BB = F.front();
  //    auto &I = BB.front();
  //    Module *pM = F.getParent();
  //    IRBuilder<> Builder(&I);

  //    Value *arg = ConstantInt::get(Type::getInt64Ty(pM->getContext()), func_num++);
  //    FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(pM->getContext()), {Type::getInt64Ty(pM->getContext())}, false);
  //    auto print = F.getParent()->getOrInsertFunction("dpt_print_func", FuncTypeA);
  //    Builder.CreateCall(print, {arg});

	//		for (auto &BB: F) {
	//			for (auto &I: BB) {
	//				if (dyn_cast<ReturnInst>(&I)) {
	//					IRBuilder<> BuilderB(&I);

	//					Value *arg = ConstantInt::get(Type::getInt64Ty(pM->getContext()), func_num-1);
	//					FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(pM->getContext()), {Type::getInt64Ty(pM->getContext())}, false);
	//					auto print = F.getParent()->getOrInsertFunction("dpt_print_func_ret", FuncTypeA);
	//					BuilderB.CreateCall(print, {arg});
	//					break;
	//				}
	//			}
	//		}

  //  }
  //}

	// Insert dpt_set() to init configuration
	auto &I = entry->front().front();
	IRBuilder<> Builder(&I);

	Value *config = nullptr;
  Value *test_mode = nullptr;
  Value *spec_mode = nullptr;
	Value *fault_mode = nullptr;
  Value *max_num_ways = nullptr;

	if (DPT_H)
		config = ConstantInt::get(Type::getInt64Ty(*C), 1);
	else if (DPT_C)
		config = ConstantInt::get(Type::getInt64Ty(*C), 2);
	else if (DPT_F)
		config = ConstantInt::get(Type::getInt64Ty(*C), 3);
	else
		config = ConstantInt::get(Type::getInt64Ty(*C), 0);

	if (llvm::DPT::getDptTestMode() == DptTest::Mode0)
		test_mode = ConstantInt::get(Type::getInt64Ty(*C), 0);
	else if (llvm::DPT::getDptTestMode() == DptTest::Mode1)
		test_mode = ConstantInt::get(Type::getInt64Ty(*C), 1);
	else if (llvm::DPT::getDptTestMode() == DptTest::Mode2)
		test_mode = ConstantInt::get(Type::getInt64Ty(*C), 2);

	if (llvm::DPT::getDptSpecMode() == DptSpec::SpecEnable)
		spec_mode = ConstantInt::get(Type::getInt64Ty(*C), 0);
	else
		spec_mode = ConstantInt::get(Type::getInt64Ty(*C), 1);

	if (llvm::DPT::getDptFaultMode() == DptFault::FaultEnable)
		fault_mode = ConstantInt::get(Type::getInt64Ty(*C), 0);
	else
		fault_mode = ConstantInt::get(Type::getInt64Ty(*C), 1);

	if (llvm::DPT::getDptMaxWaysNum() == DptMaxWays::MaxWays8)
		max_num_ways = ConstantInt::get(Type::getInt64Ty(*C), 8);
	else if (llvm::DPT::getDptMaxWaysNum() == DptMaxWays::MaxWays16)
		max_num_ways = ConstantInt::get(Type::getInt64Ty(*C), 16);
	else if (llvm::DPT::getDptMaxWaysNum() == DptMaxWays::MaxWays32)
		max_num_ways = ConstantInt::get(Type::getInt64Ty(*C), 32);
	else if (llvm::DPT::getDptMaxWaysNum() == DptMaxWays::MaxWays64)
		max_num_ways = ConstantInt::get(Type::getInt64Ty(*C), 64);
	else if (llvm::DPT::getDptMaxWaysNum() == DptMaxWays::MaxWays128)
		max_num_ways = ConstantInt::get(Type::getInt64Ty(*C), 128);
	else if (llvm::DPT::getDptMaxWaysNum() == DptMaxWays::MaxWays256)
		max_num_ways = ConstantInt::get(Type::getInt64Ty(*C), 256);

	//FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt64Ty(*C)}, false);
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C),
		{Type::getInt64Ty(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);

	auto init = M.getOrInsertFunction("__dpt_set", FuncTypeA);
	Builder.CreateCall(init, {config, max_num_ways, test_mode, spec_mode, fault_mode});
}

void DataPtrTagPass::printFuncAddr(Module &M) {
	auto &I = entry->front().front();
	IRBuilder<> Builder(&I);

	//for (auto &F : M) {
	//	if (&F && !F.isDeclaration()) {
	//		Constant *name = ConstantDataArray::getString(*C, F.getName(), true);

	//		GlobalVariable* pGV = new GlobalVariable(M, 
  //      /*Type=*/ name->getType(),
  //      /*isConstant=*/ true,
  //      /*Linkage=*/ GlobalValue::PrivateLinkage,
  //      /*Initializer=*/ 0, // has initializer, specified below
  //      /*Name=*/ ".func_name");
	//		pGV->setAlignment(Align(1));
	//		pGV->setInitializer(name);

	//		auto castA = Builder.CreateCast(Instruction::BitCast, pGV, Type::getInt8PtrTy(*C));
	//		auto castB = Builder.CreateCast(Instruction::BitCast, &F, Type::getInt8PtrTy(*C));
	//		FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C)}, false);
	//		auto print = M.getOrInsertFunction("__print_func", FuncTypeA);
	//		Builder.CreateCall(print, {castA, castB});
	//	}
	//}

  for (auto &F : M) {
		for (auto &BB: F) {
			for (auto &I: BB) {
				if (auto pCB = dyn_cast<CallBase>(&I)) {
					auto callee = pCB->getCalledFunction();
					//if (callee && callee->getName().find("llvm.") != string::npos)
					//	continue;

					IRBuilder<> Builder(&I);

					Value *arg = ConstantInt::get(Type::getInt64Ty(M.getContext()), temp_cnt++);
					FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt64Ty(M.getContext())}, false);
					auto print = F.getParent()->getOrInsertFunction("dpt_print_call", FuncTypeA);
					Builder.CreateCall(print, {arg});
				}
			}
		}
	}

	if (false) {
	//if (true) {
		for (auto &F: M) {
			for (auto &BB: F) {
				for (auto &I: BB) {
					bool found = false;
					Value *ptr = nullptr;

					if (LoadInst *pLI = dyn_cast<LoadInst>(&I)) {
						//found = true;
						//ptr = pLI->getOperand(0);
						Value *ptr_op = pLI->getPointerOperand();
						Value *val_op = pLI;
						IRBuilder<> Builder(I.getNextNode());
						Value *castB = nullptr;
						if (val_op->getType()->isPointerTy()) {
							castB = Builder.CreatePtrToInt(val_op, Type::getInt64Ty(*C));
						//} else if (val_op->getType()->isFloatTy() || val_op->getType()->isDoubleTy() || v) {
						} else if (val_op->getType()->isFPOrFPVectorTy()) {
							castB = Builder.CreateFPToSI(val_op, Type::getInt64Ty(*C));
						} else {
							castB = Builder.CreateZExt(val_op, Type::getInt64Ty(*C));
						}
						Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
						FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);
						auto check = M.getOrInsertFunction("__print_load", FuncTypeA);
						Builder.CreateCall(check, {ptr_op, castB, num}, "");
					} else if (StoreInst *pSI = dyn_cast<StoreInst>(&I)) {
						//found = true;
						//ptr = pSI->getOperand(1);
						Value *ptr_op = pSI->getPointerOperand();
						Value *val_op = pSI->getValueOperand();

						IRBuilder<> Builder(&I);
						Value *castB = nullptr;
						if (val_op->getType()->isPointerTy()) {
							castB = Builder.CreatePtrToInt(val_op, Type::getInt64Ty(*C));
						//} else if (val_op->getType()->isFloatTy() || val_op->getType()->isDoubleTy()) {
						} else if (val_op->getType()->isFPOrFPVectorTy()) {
							castB = Builder.CreateFPToSI(val_op, Type::getInt64Ty(*C));
						} else {
							castB = Builder.CreateZExt(val_op, Type::getInt64Ty(*C));
						}
						Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
						FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);
						auto check = M.getOrInsertFunction("__print_store", FuncTypeA);
						Builder.CreateCall(check, {ptr_op, castB, num}, "");
					}

					//if (found) {
					//	IRBuilder<> Builder(&I);
					//	auto castA = Builder.CreateCast(Instruction::BitCast, ptr, Type::getInt8PtrTy(*C));
					//	//FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C)}, false);
					//	//auto check = M.getOrInsertFunction("__check", FuncTypeA);
					//	//auto callA = Builder.CreateCall(check, {castA}, "");
					//	Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
					//	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
					//	auto check = M.getOrInsertFunction("__print_value", FuncTypeA);
					//	Builder.CreateCall(check, {castA, num}, "");
					//}
				}
			}
		}
	}
}

void DataPtrTagPass::handleWrapperFunctions(Module &M) {
  for (auto &F: M) {
	  if (F.getName() == "siglongjmp" || F.getName() == "longjmp") {
			auto fty = F.getFunctionType();
			auto wrap = Function::Create(fty, Function::ExternalLinkage, "__longjmp_wrapper", M);
			//auto pBB = BasicBlock::Create(*C, "", wrap, nullptr);
			F.replaceAllUsesWith(wrap);
		}
	}
}
