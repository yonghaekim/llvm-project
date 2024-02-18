#include "llvm/ACFI/PAPass.h"
#include "llvm/ACFI/ACFI.h"
#include "llvm/IR/InlineAsm.h"

#define PARTS_USE_SHA3

extern "C" {
#include "../PARTS-sha3/include/sha3.h"
}

#define ASSERT(x) if (!(x)) *(voidptr) = 0;

char PAPass::ID = 0;
static RegisterPass<PAPass> X("pa", "PA pass");

Pass *llvm::ACFI::createPAPass() { return new PAPass(); }

bool PAPass::runOnModule(Module &M) {
	errs() << "Start PA pass!\n";

	init(M);
  auto const acfiInstType = llvm::ACFI::getAcfiInstType();
  auto const acfiQemuMode = llvm::ACFI::getAcfiQemuMode();

	//printFuncNum(M);

	QEMU = (acfiQemuMode == AcfiQemuEn::Enable);

  if (acfiInstType == AcfiType::PA) {
		findWhiteSet(M);
		replaceUsers(M);
  	handleGlobalVariables(M);
		handleIndirectCalls(M);
	}

	if (QEMU) {
	  printFuncAddr(M);
	}

	//for (auto &F: M) {
  //  for (auto &BB: F) {
  //    for (auto &I: BB) {
	//			bool found = false;
	//			Value *ptr = nullptr;

  //      if (LoadInst *pLI = dyn_cast<LoadInst>(&I)) {
	//				found = true;
	//				ptr = pLI->getOperand(0);
  //      } else if (StoreInst *pSI = dyn_cast<StoreInst>(&I)) {
	//				found = true;
	//				ptr = pSI->getOperand(1);
	//			}

	//			if (found) {
	//				IRBuilder<> Builder(&I);
	//				auto castA = Builder.CreateCast(Instruction::BitCast, ptr, Type::getInt8PtrTy(*C));
	//				//FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C)}, false);
	//				//auto check = M.getOrInsertFunction("__check", FuncTypeA);
	//				//auto callA = Builder.CreateCall(check, {castA}, "");
	//				Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
	//				FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
	//				auto check = M.getOrInsertFunction("__print_value", FuncTypeA);
	//				Builder.CreateCall(check, {castA, num}, "");
	//			}
	//		}
	//	}
	//}

	errs() << "statNumIndirectCall: " << statNumIndirectCall << "\n";

	return false; // function_modified = false
}

void PAPass::findWhiteSet(Module &M) {
	for (auto &F: M) {
		// No need to handle llvm intrinsics
		if (F.getName().find("llvm.") != string::npos)
			continue;

    // No need to handle startup code
		if (F.getSection().find(".text.startup") != std::string::npos)
      continue;

		// TODO .... start from here....
		//if (F.isVarArg())
		//	continue;

		// Replacing personlaity function with a JTE causes an error (need to debug)
		if (F.getName().find("__gxx_personality_v0") != std::string::npos)
      continue;

		for (auto pU: F.users()) {
			if (auto pSI = dyn_cast<StoreInst>(pU)) {
				if (pSI->getValueOperand() == &F)
					white_set.insert(&F);
			} else if (auto pCB = dyn_cast<CallBase>(pU)) {
				if (pCB->getCalledFunction() != &F)
					white_set.insert(&F);
			} else if (dyn_cast<SelectInst>(pU)) {
				white_set.insert(&F);
			} else if (dyn_cast<PHINode>(pU)) {
				white_set.insert(&F);
			} else if (dyn_cast<GlobalVariable>(pU)) {
				white_set.insert(&F);
			} else if (dyn_cast<BitCastOperator>(pU)) {
				white_set.insert(&F);
			} else if (dyn_cast<Constant>(pU)) {
				white_set.insert(&F);
			} else if (dyn_cast<ICmpInst>(pU)) {
				white_set.insert(&F);
			}
		}
	}

	errs() << "Size of white set: " << white_set.size() << "\n";
	for (auto pF: white_set) {
		errs() << "--" << pF->getName() << "\n";
	}
}

void PAPass::replaceUsers(Module &M) {
	for (auto pF: white_set) {
		if (pF->isVarArg())
			errs() << "Found VarArg! " << pF->getName() << "\n";

		//errs() << "pF: " << pF->getName() << "\n";

		unsigned num_param = pF->getFunctionType()->getNumParams();
		auto context = ConstantInt::get(Type::getInt64Ty(*C), num_param);
    //auto context = getTypeIDConstantFrom(*(pF->getFunctionType()), *C);

  	map<Value*,set<Value*>*> user_map;
    findUsers(M, pF, user_map);

    for (auto x: user_map) {
      auto pV = x.first;
      auto user_set = x.second;

      if (!user_set)
        continue;

      for (auto pU: *user_set) {
        if (auto pPN = dyn_cast<PHINode>(pU)) {
          handlePHINode(M, pPN, pV, context);
        } else if (auto pI = dyn_cast<Instruction>(pU)) {
          handleInstruction(M, pI, pV, context);
        //} else if (auto pGV = dyn_cast<GlobalVariable>(pU)) {
        //  handleGlobalVariable(M, pGV, pV, context);
        //} else if (auto pC = dyn_cast<Constant>(pU)) {
        //  handleConstant(M, pC, pV, context);
        }
      }
    }
  }
}

void PAPass::findUsers(Module &M, Value *pV, map<Value*,set<Value*>*> &user_map) {
	for (auto pU: pV->users()) {
		//errs() << "pU: "; pU->dump();

		if (auto pCB = dyn_cast<CallBase>(pU)) {
			if (pCB->getCalledOperand() != pV) {
				//errs() << "Insert(1): "; pCB->dump();
        if (!user_map[pV])
          user_map[pV] = new set<Value *>;

				user_map[pV]->insert(pCB);
			}
		} else if (auto pI = dyn_cast<Instruction>(pU)) {
      if (!user_map[pV])
        user_map[pV] = new set<Value *>;

			user_map[pV]->insert(pI);
			//errs() << "(2) pV: "; pV->dump();
		//} else if (auto pGV = dyn_cast<GlobalVariable>(pU)) {
    //  if (!user_map[pV])
    //    user_map[pV] = new set<Value *>;

		//	user_map[pV]->insert(pGV);
		//} else if (auto pC = dyn_cast<Constant>(pU)) {
		//	findUsers(M, pC, user_map);
		} else if (auto pGA = dyn_cast<GlobalAlias>(pU)) {
			//errs() << "pGA: "; pU->dump();
			findUsers(M, pGA, user_map);
		} else if (auto pOP = dyn_cast<Operator>(pU)) {
			findUsers(M, pOP, user_map);
		} else {
      //errs() << "Else pU: "; pU->dump();
    }
	}
}

void PAPass::handlePHINode(Module &M, PHINode *pPN, Value *pV, Value *context) {
	unsigned num = pPN->getNumIncomingValues();

	for (unsigned i=0; i<num; i++) {
		if (pPN->getIncomingValue(i) == pV) {
			IRBuilder<> Builder(&(pPN->getIncomingBlock(i)->back()));
			auto callA = insertTagc(M, &Builder, pV, context);

      Value *castA = nullptr;
      if (pV->getType()->isPointerTy())
        castA = Builder.CreateCast(Instruction::BitCast, callA, pV->getType());
      else
        castA = Builder.CreatePtrToInt(callA, pV->getType());

      // To handle the case where pPN has same incoming blocks with same incoming value...
      for (unsigned j=i; j<num; j++) {
        if (pPN->getIncomingValue(j) == pV &&
            pPN->getIncomingBlock(i) == pPN->getIncomingBlock(j)) {
          pPN->setIncomingValue(j, castA);
        }
      }
		}
	}
}

void PAPass::handleInstruction(Module &M, Instruction *pI, Value *pV, Value *context) {
	//errs() << "pI: "; pI->dump();

	if (auto pCB = dyn_cast<CallBase>(pI)) {
		if (auto pF = dyn_cast<Function>(pCB->getCalledOperand())) {
			if (pF->isDeclaration())
//getName() == "_obstack_begin")
				return;
		}		
	}

	unsigned n = pI->getNumOperands();

  for (unsigned i=0; i<n; i++) { //TODO n-1? n?
    if (pI->getOperand(i) == pV) {
    	IRBuilder<> Builder(pI);
      auto callA = insertTagc(M, &Builder, pV, context);

      Value *castA = nullptr;
      if (pV->getType()->isPointerTy())
        castA = Builder.CreateCast(Instruction::BitCast, callA, pV->getType());
      else
        castA = Builder.CreatePtrToInt(callA, pV->getType());

      pI->setOperand(i, castA);
    }
  }
}

void PAPass::handleGlobalVariables(Module &M) {
	set<GlobalVariable*> gv_set;

	for (auto &G : M.getGlobalList()) {
		GlobalVariable *pGV = dyn_cast<GlobalVariable>(&G);

		//errs() << "(a) pGV->dump(): "; pGV->dump();
		if (pGV->getName().find(".str") != 0 && pGV->hasInitializer()) {
			gv_set.insert(pGV);
		}
  }

	for (auto pGV: gv_set) {
		//errs() << "pGV->dump(): "; pGV->dump();
		handleGlobalVariable(M, pGV);
	}
}

void PAPass::handleGlobalVariable(Module &M, GlobalVariable *pGV) {
	Constant *pC = pGV->getInitializer();
	Type *ty = pC->getType();
	vector<Value*> *indices = new vector<Value*>;

  if (ty->isArrayTy() || ty->isStructTy()) {
  	indices->push_back(ConstantInt::get(Type::getInt32Ty(*C), 0));
    getIndices(M, pGV, pC, indices);
  } else {
    handleFunctionOrOperator(M, pGV, pC, indices);
  }

	pGV->setConstant(false);

  delete indices;
}

void PAPass::getIndices(Module &M, GlobalVariable *pGV, Constant *pConst, vector<Value*> *indices) {
	Type *ty = pConst->getType();
  unsigned n = 0;

  if (auto sty = dyn_cast<StructType>(ty))
    n = sty->getNumElements();
  else if (auto aty = dyn_cast<ArrayType>(ty))
    n = aty->getNumElements();

	for (unsigned i=0; i<n; i++) {
		auto elmt = pConst->getAggregateElement(i);
    auto ety = elmt->getType();
		vector<Value*> *indices_new = new vector<Value*>;

		// Copy indices
		for (auto pV: *indices)
			indices_new->push_back(pV);

		indices_new->push_back(ConstantInt::get(Type::getInt32Ty(*C), i));

    if (ety->isArrayTy() || ety->isStructTy()) {
      getIndices(M, pGV, elmt, indices_new);
    } else {
      handleFunctionOrOperator(M, pGV, elmt, indices_new);
    }
    //} else if (auto pF = dyn_cast<Function>(elmt)) {
    //  // Found a (white-listed) function in the global variable
  	//	if (white_set.find(pF) != white_set.end()) {
    //    //errs() << "(1) pF: " << pF->getName() << "\n";
    //    overwriteFuncAddr(M, pGV, pF, elmt, indices_new);
    //  }
    //} else if (auto pOP = dyn_cast<Operator>(elmt)) {
    //  //errs() << "pOP: "; elmt->dump();   
    //  //pOP->getOperand(0)->dump();
    //  Value *pV = nullptr;
    //  auto op = pOP;

    //  do {
    //    pV = op->getOperand(0);
    //    if (dyn_cast<Operator>(pV)) {
    //      op = dyn_cast<Operator>(pV);
    //    } else { break; }
    //  } while (1);

    //  if (auto pF = dyn_cast<Function>(pV)) {
    //    if (white_set.find(pF) != white_set.end()) {
    //      // Found a (white-listed) function in the global variable
    //      //errs() << "(2) pF: " << pF->getName() << "\n";
    //      overwriteFuncAddr(M, pGV, pF, elmt, indices_new);
    //    }
    //  }
    //}

		delete indices_new;
  }
}

void PAPass::handleFunctionOrOperator(Module &M, GlobalVariable *pGV, Constant *pC, vector<Value*> *indices) {
  if (auto pF = dyn_cast<Function>(pC)) {
    // Found a (white-listed) function in the global variable
    if (white_set.find(pF) != white_set.end()) {
      //errs() << "(1) pF: " << pF->getName() << "\n";
      overwriteFuncAddr(M, pGV, pF, pC, indices);
    }
  } else if (auto pOP = dyn_cast<Operator>(pC)) {
    //errs() << "pOP: "; pC->dump();   
    //pOP->getOperand(0)->dump();
    Value *pV = nullptr;
    auto op = pOP;

    do {
      pV = op->getOperand(0);
      if (dyn_cast<Operator>(pV)) {
        op = dyn_cast<Operator>(pV);
      } else { break; }
    } while (1);

    if (auto pF = dyn_cast<Function>(pV)) {
      if (white_set.find(pF) != white_set.end()) {
        // Found a (white-listed) function in the global variable
        //errs() << "(2) pF: " << pF->getName() << "\n";
        overwriteFuncAddr(M, pGV, pF, pC, indices);
      }
    }
  }
}

void PAPass::overwriteFuncAddr(Module &M, GlobalVariable *pGV, Function *pF, Value *pV, vector<Value*> *indices) {
	IRBuilder<> Builder(&(init_acfi->back().back()));

	unsigned num_param = pF->getFunctionType()->getNumParams();
	auto context = ConstantInt::get(Type::getInt64Ty(*C), num_param);
  //auto context = getTypeIDConstantFrom(*(pF->getFunctionType()), *C);
	//auto context = ConstantInt::get(Type::getInt64Ty(*C), 0);

  // For store val_op, ptr_op, do...
  // tagc  pV, pV, context
  // store pV, ptr_op
	auto ety = dyn_cast<PointerType>(pGV->getType())->getPointerElementType();
	auto gepA = Builder.CreateGEP(ety, pGV, *indices);
  // 1.
	auto callB = insertTagc(M, &Builder, pV, context);
	auto castB = Builder.CreateCast(Instruction::BitCast, callB, pV->getType());
	auto store = Builder.CreateStore(castB, gepA);
	store->setAlignment(Align(8));
}

Value *PAPass::insertTagc(Module &M, IRBuilder<> *Builder, Value *pV, Value *context) {
  FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
  //auto castA = Builder->CreateCast(Instruction::BitCast, pV, Type::getInt8PtrTy(*C));
	Value *castA = nullptr;

	if (pV->getType()->isPointerTy())
		castA = Builder->CreateCast(Instruction::BitCast, pV, Type::getInt8PtrTy(*C));
	else
		castA = Builder->CreateIntToPtr(pV, Type::getInt8PtrTy(*C));

  auto tagc = M.getOrInsertFunction("__tagc", FuncTypeA);
  //auto tagc = QEMU? M.getOrInsertFunction("__tagc", FuncTypeA) :
  //                  Intrinsic::getDeclaration(&M, Intrinsic::pa_tagc, {Type::getInt8PtrTy(*C)});

  return Builder->CreateCall(tagc, {castA, context}, "");
}

Value *PAPass::insertAutc(Module &M, IRBuilder<> *Builder, Value *pV, Value *context) {
  FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);
  Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);

  //if (QEMU) {
    auto autc = M.getOrInsertFunction("__autc", FuncTypeA);
    return Builder->CreateCall(autc, {pV, context, num}, "");
  //} else {
  //  auto autc = Intrinsic::getDeclaration(&M, Intrinsic::acfi_autc, {Type::getInt8PtrTy(*C)});
  //  return Builder->CreateCall(autc, {pV, context}, "");
  //}
}

void PAPass::handleIndirectCalls(Module &M) {
	set<CallBase*> call_set;

	for (auto &F: M) {
    // Skip startup code
		if (F.getSection().find(".text.startup") != std::string::npos)
      continue;

		for (auto &BB : F) {
			Function *caller = &F;

			for (auto &I : BB) {
				if (auto pCB = dyn_cast<CallBase>(&I)) {
					if (pCB->isIndirectCall()) {
						call_set.insert(pCB);
						statNumIndirectCall++;
					}
				}
			}
		}
	}

	for (auto pCB: call_set) {
		handleIndirectCall(M, pCB);
	}
}

void PAPass::handleIndirectCall(Module &M, CallBase *pCB) {
  //errs() << "pI->dump(): "; pI->dump();
	Value* callee = pCB->getCalledOperand();
	FunctionType *fty0 = pCB->getFunctionType();

	//bool chk = false;
	//for (auto &F: M) {
	//	if (F.getFunctionType() == fty0) {
	//		errs() << F.getName() << "\n";
	//		chk = true;
	//		break;
	//	}
	//}

	//if (!chk) {
	//	//errs() << "Found unknown function type!\n";
	//	fty0->dump();
	//	return;
	//} else {
	//	//errs() << "Found function type!\n";	
	//	//errs() << "temp_cnt: " << temp_cnt << "\n";
	//	fty0->dump();
	//}

	unsigned num_param = 0;

	for (auto itr = pCB->arg_begin() ; itr != pCB->arg_end(); itr++) {
		num_param++;
	}

	auto context = ConstantInt::get(Type::getInt64Ty(*C), num_param);
  //auto context = getTypeIDConstantFrom(*fty0, *C);
	//auto context = ConstantInt::get(Type::getInt64Ty(*C), 0);

	IRBuilder<> Builder(pCB);
	auto castA = Builder.CreateCast(Instruction::BitCast, callee, Type::getInt8PtrTy(*C));
  auto callA = insertAutc(M, &Builder, castA, context);
	auto castB = Builder.CreateCast(Instruction::BitCast, callA, callee->getType());
	pCB->setCalledOperand(castB);

	//if (true) {
	////if (false) {
	//	// This is for debugging
	//	//FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
	//	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
	//	Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
	//	//auto print = M.getOrInsertFunction("__print_value", FuncTypeA);
	//	auto check = M.getOrInsertFunction("__check_value", FuncTypeA);

	//	//Builder.CreateCall(print, {castA, num}); //
	//	//Builder.CreateCall(print, {callA, num}); //
	//	auto castA = Builder.CreateCast(Instruction::BitCast, callee, Type::getInt8PtrTy(*C));
	//	//Builder.CreateCall(print, {castA, num}); //
	//	//auto castB = Builder.CreateCast(Instruction::BitCast, jt, Type::getInt8PtrTy(*C));
	//	//Builder.CreateCall(print, {castB, num}); //
	//	auto castC = Builder.CreateCast(Instruction::BitCast, jte, Type::getInt8PtrTy(*C));
	//	Builder.CreateCall(check, {castA, castC, num}); //
	//}
}

void PAPass::printFuncAddr(Module &M) {
	IRBuilder<> Builder(&(init_acfi->front().front()));

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

	for (auto pF: white_set) {
		Constant *name = ConstantDataArray::getString(*C, pF->getName(), true);

		GlobalVariable* pGV = new GlobalVariable(M, 
			/*Type=*/ name->getType(),
			/*isConstant=*/ true,
			/*Linkage=*/ GlobalValue::PrivateLinkage,
			/*Initializer=*/ 0, // has initializer, specified below
			/*Name=*/ ".func_name");
		pGV->setAlignment(Align(1));
		pGV->setInitializer(name);

		auto castA = Builder.CreateCast(Instruction::BitCast, pGV, Type::getInt8PtrTy(*C));
		auto castB = Builder.CreateCast(Instruction::BitCast, pF, Type::getInt8PtrTy(*C));
		FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C)}, false);
		auto print = M.getOrInsertFunction("__print_func", FuncTypeA);
		Builder.CreateCall(print, {castA, castB});
	}
}

void PAPass::printFuncNum(Module &M) {
  for (auto &F : M) {
    if (&F && !F.isDeclaration()) {
      auto &BB = F.front();
      auto &I = BB.front();
      Module *pM = F.getParent();
      IRBuilder<> Builder(&I);

      Value *arg = ConstantInt::get(Type::getInt64Ty(pM->getContext()), func_num++);
      FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(pM->getContext()), {Type::getInt64Ty(pM->getContext())}, false);
      auto print = F.getParent()->getOrInsertFunction("cpt_print_func", FuncTypeA);
      Builder.CreateCall(print, {arg});

			for (auto &BB: F) {
				for (auto &I: BB) {
					if (dyn_cast<ReturnInst>(&I)) {
						IRBuilder<> BuilderB(&I);

						Value *arg = ConstantInt::get(Type::getInt64Ty(pM->getContext()), func_num-1);
						FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(pM->getContext()), {Type::getInt64Ty(pM->getContext())}, false);
						auto print = F.getParent()->getOrInsertFunction("cpt_print_func_ret", FuncTypeA);
						BuilderB.CreateCall(print, {arg});
						break;
					}
				}
			}
    }
  }

  for (auto &F : M) {
		for (auto &BB: F) {
			for (auto &I: BB) {
				if (auto pCB = dyn_cast<CallBase>(&I)) {
					auto callee = pCB->getCalledFunction();
					//if (callee && callee->getName().find("llvm.") != string::npos)
					//	continue;

					IRBuilder<> Builder(&I);

					Value *arg = ConstantInt::get(Type::getInt64Ty(M.getContext()), call_num++);
					FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt64Ty(M.getContext())}, false);
					auto print = F.getParent()->getOrInsertFunction("cpt_print_call", FuncTypeA);
					Builder.CreateCall(print, {arg});
				}
			}
		}
	}
}

void PAPass::init(Module &M) {
	for (auto &F : M) {
		if (&F && F.getName() == "main") {
		  C = &F.getContext();
			DL = &F.getParent()->getDataLayout();
			main = &F;
			break;
		}
	}

	// Insert __init_acfi() to program entry
	// TODO: need to insert it before global constructors in C++
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), false);
	init_acfi = Function::Create(FuncTypeA, Function::ExternalLinkage, "__init_acfi", M);
	auto pBB = BasicBlock::Create(*C, "", init_acfi, nullptr);
	ReturnInst::Create(*C, pBB);

	IRBuilder<> BuilderA(&(main->front().front()));
	BuilderA.CreateCall(FuncTypeA, init_acfi);

	// Insert __acfi_set() to init configuration
	IRBuilder<> BuilderB(&(init_acfi->front().front()));
	auto config = ConstantInt::get(Type::getInt64Ty(*C), 0);

	FunctionType *FuncTypeB = FunctionType::get(Type::getVoidTy(*C), Type::getInt64Ty(*C), false);
	auto acfi_set = M.getOrInsertFunction("__acfi_set", FuncTypeB);
	BuilderB.CreateCall(acfi_set, config);
}

void PAPass::buildTypeString(const Type *T, llvm::raw_string_ostream &O) {
  if (T->isPointerTy()) {
    O << "ptr.";
    buildTypeString(T->getPointerElementType(), O);
  } else if (T->isStructTy()) {
		auto sty = dyn_cast<StructType>(T);
		std::regex e("^(\\w+\\.\\w+)(\\.\\w+)?$");

		if (sty->isLiteral()) {
			O << std::regex_replace("str.", e, "$1");
		} else {
			auto structName = dyn_cast<StructType>(T)->getStructName();
			O << std::regex_replace(structName.str(), e, "$1");
		}
  } else if (T->isArrayTy()) {
    O << "arr.";
    buildTypeString(T->getArrayElementType(), O);
  } else if (T->isFunctionTy()) {
    auto FuncTy = dyn_cast<FunctionType>(T);
    O << "f.";
    buildTypeString(FuncTy->getReturnType(), O);

    for (auto p = FuncTy->param_begin(); p != FuncTy->param_end(); p++) {
      buildTypeString(*p, O);
    }
  } else if (T->isVectorTy()) {
    O << "vec." << dyn_cast<VectorType>(T)->getElementCount();
    buildTypeString(dyn_cast<VectorType>(T)->getElementType(), O);
  } else if (T->isVoidTy()) {
    O << "v";
  } else {
    /* Make sure we've handled all cases we want to */
    assert(T->isIntegerTy() || T->isFloatingPointTy());
    T->print(O);
  }
}


uint64_t PAPass::getTypeIDFor(const Type *T) {
  if (!T->isPointerTy())
    return 0; // Not a pointer, hence no type ID for this one

  // TODO: This should perform caching, so calling the same Type will not
  // reprocess the stuff. Use a Dictionary-like ADT is suggested.
  decltype(TypeIDCache)::iterator id;
  if ((id = TypeIDCache.find(T)) != TypeIDCache.end())
    return id->second;

  uint64_t theTypeID = 0;
  std::string buf;
  llvm::raw_string_ostream typeIdStr(buf);

  buildTypeString(T, typeIdStr);
  typeIdStr.flush();

  // Prepare SHA3 generation
  auto rawBuf = buf.c_str();
  mbedtls_sha3_context sha3_context;
  mbedtls_sha3_type_t sha3_type = MBEDTLS_SHA3_256;
  mbedtls_sha3_init(&sha3_context);

  // Prepare input and output variables
  auto *input = reinterpret_cast<const unsigned char *>(rawBuf);
  auto *output = new unsigned char[32]();

  // Generate hash
  auto result = mbedtls_sha3(input, buf.length(), sha3_type, output);
  if (result != 0)
    llvm_unreachable("SHA3 hashing failed :(");
  memcpy(&theTypeID, output, sizeof(theTypeID));
  // TODO need to fix delete[] output;

  TypeIDCache.emplace(T, theTypeID);

  return theTypeID;
}

Constant *PAPass::getTypeIDConstantFrom(const Type &T, LLVMContext &C) {
  return Constant::getIntegerValue(Type::getInt64Ty(C),
                                   APInt(64, getTypeIDFor(&T)));
}
