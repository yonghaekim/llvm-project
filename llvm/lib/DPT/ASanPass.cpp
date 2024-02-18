#include "llvm/DPT/ASanPass.h"
#include "llvm/DPT/DPT.h"

#define ASSERT(x) if (!(x)) *(voidptr) = 0;

char ASanPass::ID = 0;
static RegisterPass<ASanPass> X("asan", "Data-pointer tagging pass");

Pass *llvm::DPT::createASanPass() { return new ASanPass(); }

bool ASanPass::runOnModule(Module &M) {
	errs() << "Start ASan Pass!\n";
	init(M);

	handleGlobalVariables(M);
	handleAllocas(M);
	handleMemInsts(M);
	insertDptSet(M);

	return true; // function_modified = true
}

void ASanPass::handleGlobalVariables(Module &M) {
	set<GlobalVariable *> gv_set;

	for (auto &G : M.getGlobalList()) {
		GlobalVariable *pGV = dyn_cast<GlobalVariable>(&G);
		Type *ty = pGV->getValueType();

		// Skip ".str" constants
		if ((pGV->isConstant() && pGV->getName().find(".str") == 0) ||
				(pGV->use_empty() || pGV->getLinkage() == GlobalValue::LinkOnceODRLinkage) || //TODO
				(!pGV->hasInitializer() || pGV->isDeclaration()) ||
				(!ty->isArrayTy() && !IsStructTyWithArray(ty)))
			continue;

		gv_set.insert(pGV);
  }

	for (auto pGV: gv_set)
		handleGlobalVariable(M, pGV);
}

void ASanPass::handleGlobalVariable(Module &M, GlobalVariable *pGV) {
	// 32B redzone, [32 x i8]
	auto aty = ArrayType::get(Type::getInt8Ty(*C), 32);
	auto zero = ConstantInt::get(Type::getInt64Ty(*C), 0);
	auto pCA = ConstantArray::get(aty, {zero});

	GlobalVariable *redzoneA = new GlobalVariable(M, 
    /*Type=*/ pCA->getType(),
    /*isConstant=*/ true,
    /*Linkage=*/ GlobalValue::PrivateLinkage,
    /*Initializer=*/ 0, // has initializer, specified below
    /*Name=*/ ".redzoneA");
	redzoneA->setAlignment(Align(1));
	redzoneA->setInitializer(pCA);

	GlobalVariable *redzoneB = new GlobalVariable(M, 
    /*Type=*/ pCA->getType(),
    /*isConstant=*/ true,
    /*Linkage=*/ GlobalValue::PrivateLinkage,
    /*Initializer=*/ 0, // has initializer, specified below
    /*Name=*/ ".redzone");
	redzoneB->setAlignment(Align(1));
	redzoneB->setInitializer(pCA);

	Type *ty = pGV->getValueType();
	auto size = DL->getTypeSizeInBits(ty);
	Value *arg = ConstantInt::get(Type::getInt64Ty(*C), size / 8);

	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
  IRBuilder<> Builder(&(entry->back().back()));
	auto init_redzone = M.getOrInsertFunction("__asan_init_redzone", FuncTypeA);
	Builder.CreateCall(init_redzone, {pGV, arg});
}

void ASanPass::handleAllocas(Module &M) {
	set<AllocaInst *> alloca_set;

	for (auto &F : M) {
		for (auto &BB : F) {
			//if (!BB.isEntryBlock())
			//	continue;
			for (auto &I : BB) {
				if (auto pAI = dyn_cast<AllocaInst>(&I)) {
					Type *ty = pAI->getAllocatedType();
					Function *pF = pAI->getFunction();

					if (ty->isArrayTy() || IsStructTyWithArray(ty)) { //TODO check...
					//if (ty->isArrayTy()) {
						alloca_set.insert(pAI);
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
	}

	for (auto pAI: alloca_set)
		handleAlloca(M, pAI);
}

void ASanPass::handleAlloca(Module &M, AllocaInst *pAI) {
	Type *ty = pAI->getAllocatedType();
  auto size = pAI->getAllocationSizeInBits(*DL);
	Value *arg = ConstantInt::get(Type::getInt64Ty(*C), (*size) / 8);

	// Allocate redzones
	auto aty = ArrayType::get(Type::getInt8Ty(*C), 32);
	auto zero = ConstantInt::get(Type::getInt64Ty(*C), 0);
	auto pCA = ConstantArray::get(aty, {zero});

  IRBuilder<> BuilderA(pAI);
	auto redzoneA = BuilderA.CreateAlloca(pCA->getType(), nullptr);

  IRBuilder<> BuilderB(pAI->getNextNode());
	auto redzoneB = BuilderB.CreateAlloca(pCA->getType(), nullptr);

	// Initialize corresponding shadow memory regions
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
	auto init_redzone = M.getOrInsertFunction("__asan_init_redzone", FuncTypeA);
	BuilderB.CreateCall(init_redzone, {pAI, arg});

  // Check Return Inst
	//bool chk = false;
	auto pF = pAI->getFunction();
	//Instruction *pIB = nullptr;
	Instruction *ret = nullptr;
	Instruction *resume = nullptr;

	for (auto &BB: *pF) {
		for (auto &I: BB) {
			if (dyn_cast<ReturnInst>(&I)) {
				//pIB = &I;
				//chk = true;
				ret = &I;
				break;
			} else if (dyn_cast<ResumeInst>(&I)) {
				//pIB = &I;
				//chk = true;
				resume = &I;
				break;
      }
		}
	}

	//if (!chk) {
	if (!ret && !resume) {
		errs() << "Not Found ReturnInst! " << pF->getName() << "\n";
		return;
	}

	if (pAI->getParent()->isEntryBlock()) {
		// Initialize corresponding shadow memory regions
    if (ret) {
  		IRBuilder<> BuilderC(ret);
  		BuilderC.CreateCall(init_redzone, {pAI, arg});
    }

    if (resume) {
  		IRBuilder<> BuilderC(resume);
  		BuilderC.CreateCall(init_redzone, {pAI, arg});
    }
	}
}

void ASanPass::handleMemInsts(Module &M) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C)}, false);
	auto check = M.getOrInsertFunction("__asan_check", FuncTypeA);
	dyn_cast<Function>(check.getCallee())->addFnAttr(Attribute::AlwaysInline);

	for (auto &F: M) {
    for (auto &BB: F) {
      for (auto &I: BB) {
        if (LoadInst *pLI = dyn_cast<LoadInst>(&I)) {
					IRBuilder<> Builder(pLI);
					auto pV = pLI->getPointerOperand();
					Builder.CreateCall(check, {pV});
        } else if (StoreInst *pSI = dyn_cast<StoreInst>(&I)) {
					IRBuilder<> Builder(pSI);
					auto pV = pSI->getPointerOperand();
					Builder.CreateCall(check, {pV});
				}
			}
		}
	}
}

bool ASanPass::IsStructTyWithArray(Type *ty) {
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

set<Type *> ASanPass::getStructTypes(Type *ty, set<Type *> type_set) {
	while (ty->isArrayTy())
		ty = ty->getArrayElementType();

	if (auto str_ty = dyn_cast<StructType>(ty)) {
		for (auto it = str_ty->element_begin(); it != str_ty->element_end(); it++) {
			Type *ety = (*it);

			if (ety->isArrayTy())
				type_set.insert(str_ty);

			//if (ety->isStructTy())
			if (IsStructTyWithArray(ety))
				type_set = getStructTypes(ety, type_set);
		}
	}

	return type_set;
}

bool ASanPass::IsStructTy(Type *ty) {
	if (ty->isStructTy())
		return true;
	else if (!ty->isArrayTy())
		return false;

	while (ty->isArrayTy()) {
		ty = ty->getArrayElementType();

		if (ty->isStructTy())
			return true;
	}

	return false;
}

void ASanPass::init(Module &M) {
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
	entry = Function::Create(FuncTypeA, Function::ExternalLinkage, "__init_asan", M);
	auto pBB = BasicBlock::Create(*C, "", entry, nullptr);
	ReturnInst::Create(*C, pBB);

	auto &BB = start->front();
	auto &I = BB.front();
	IRBuilder<> Builder(&I);		
	Builder.CreateCall(FuncTypeA, entry);

  auto const dptInstType = llvm::DPT::getDptInstType();
  auto const dptQemuMode = llvm::DPT::getDptQemuMode();
}

void ASanPass::insertDptSet(Module &M) {
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

	size_t config = 0;

	// Insert dpt_set() to init configuration
	auto &I = entry->front().front();
	IRBuilder<> Builder(&I);

	//FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt64Ty(*C)}, false);
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt64Ty(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);

	Value *num = ConstantInt::get(Type::getInt64Ty(*C), config);
  Value *thres = ConstantInt::get(Type::getInt64Ty(*C), 8);
  Value *mode = ConstantInt::get(Type::getInt64Ty(*C), 3);

	auto init = M.getOrInsertFunction("__asan_set", FuncTypeA);
	//Builder.CreateCall(init, {num});
	Builder.CreateCall(init, {num, thres, mode});
}

void ASanPass::printFuncAddr(Module &M) {
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

	//if (false) {
	if (true) {
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
						auto castA = Builder.CreateCast(Instruction::BitCast, ptr_op, Type::getInt8PtrTy(*C));
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
						Builder.CreateCall(check, {castA, castB, num}, "");
					} else if (StoreInst *pSI = dyn_cast<StoreInst>(&I)) {
						//found = true;
						//ptr = pSI->getOperand(1);
						Value *ptr_op = pSI->getPointerOperand();
						Value *val_op = pSI->getValueOperand();

						IRBuilder<> Builder(&I);
						auto castA = Builder.CreateCast(Instruction::BitCast, ptr_op, Type::getInt8PtrTy(*C));
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
						Builder.CreateCall(check, {castA, castB, num}, "");
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
