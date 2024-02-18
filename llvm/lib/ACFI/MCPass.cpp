#include "llvm/ACFI/MCPass.h"
#include "llvm/ACFI/ACFIPass.h"
#include "llvm/ACFI/ACFI.h"
#include "llvm/IR/InlineAsm.h"

#define ASSERT(x) if (!(x)) *(voidptr) = 0;

char MCPass::ID = 0;
static RegisterPass<MCPass> X("acfi-mc", "ACFI + MAC closure pass");

Pass *llvm::ACFI::createMCPass() { return new MCPass(); }

bool MCPass::runOnModule(Module &M) {
	errs() << "Start MC pass!\n";

	init(M);
  auto const acfiInstType = llvm::ACFI::getAcfiInstType();
  auto const acfiQemuMode = llvm::ACFI::getAcfiQemuMode();

	//printFuncNum(M);
	QEMU = (acfiQemuMode == AcfiQemuEn::Enable);

	handleInstructions(M);
  handleWrapperFunctions(M);

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

	return false; // function_modified = false
}

void MCPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.setPreservesAll();
  AU.addRequired<ACFIPass> ();
}

void MCPass::handleInstructions(Module &M) {
  set<StoreInst*> store_set;
  set<LoadInst*> load_set;
  set<AllocaInst*> alloca_set;
	set<CallBase*> free_set;

  for (auto &F: M) {
		if (F.getName() == "__acfi_set" ||
		    F.getName() == "__init_acfi")
			continue;

		if (F.getSection().find(".text.startup") != std::string::npos)
      continue;

    for (auto &BB: F) {
      for (auto &I: BB) {
				if (auto pSI = dyn_cast<StoreInst>(&I)) {
					auto *pty = dyn_cast<PointerType>(pSI->getValueOperand()->getType());
					if (pty && pty->getPointerElementType()->isFunctionTy())
						store_set.insert(pSI);
				} else if (auto pLI = dyn_cast<LoadInst>(&I)) {
					auto *pty = dyn_cast<PointerType>(pLI->getType());
					if (pty && pty->getPointerElementType()->isFunctionTy())
						load_set.insert(pLI);
				} else if (auto pAI = dyn_cast<AllocaInst>(&I)) {
					if (hasFuncPtrTy(pAI->getAllocatedType()))
						alloca_set.insert(pAI);
        }
			}
		}
  }

  for (auto pSI: store_set)
    handleStoreInst(M, pSI);

  for (auto pLI: load_set)
    handleLoadInst(M, pLI);

  for (auto pAI: alloca_set)
    handleAllocaInst(M, pAI);
}

void MCPass::handleStoreInst(Module &M, StoreInst *pSI) {
  Value *ptr_op = pSI->getPointerOperand();
  Value *val_op = pSI->getValueOperand();

	auto pty = dyn_cast<PointerType>(val_op->getType());
	auto fty = dyn_cast<FunctionType>(pty->getPointerElementType());
	unsigned num_param = fty->getNumParams();
	auto context = ConstantInt::get(Type::getInt64Ty(*C), num_param);
	//auto zero = ConstantInt::get(Type::getInt64Ty(*C), 0);

	IRBuilder<> Builder(pSI);

  // MAC Closure sequence
	// bset ptr_op
	// tagc val_op, val_op, ptr_op
	// store val_op, ptr_op
	auto castA = Builder.CreateCast(Instruction::BitCast, ptr_op, Type::getInt8PtrTy(*C));
	insertBsetm(M, &Builder, castA);
	auto castB = Builder.CreateCast(Instruction::BitCast, val_op, Type::getInt8PtrTy(*C));
	auto castC = Builder.CreatePtrToInt(ptr_op, Type::getInt64Ty(*C));
	auto callA = insertTagc(M, &Builder, castB, castC);
	auto castD = dyn_cast<Instruction>(Builder.CreateCast(Instruction::BitCast, callA, val_op->getType()));

	pSI->setOperand(0, castD);
}

void MCPass::handleLoadInst(Module &M, LoadInst *pLI) {
  Value *val_op = pLI;
  Value *ptr_op = pLI->getPointerOperand();

	auto pty = dyn_cast<PointerType>(val_op->getType());
	auto fty = dyn_cast<FunctionType>(pty->getPointerElementType());
	unsigned num_param = fty->getNumParams();
	auto context = ConstantInt::get(Type::getInt64Ty(*C), num_param);
	//auto zero = ConstantInt::get(Type::getInt64Ty(*C), 0);

	// In gcc_r, function type with variable arguments can be loaded
	// It is later used with the different # of arguments for indirect all
	// In such a case, we can infer the correct # of arguments by traversing def-use chain
	// and checking the second opernad of __echk()
	// Example code in gcc_r
	// %33 = load %struct.rtx_def* (%struct.rtx_def*, ...)*, %struct.rtx_def* (%struct.rtx_def*, ...)** %32
	// %34 = bitcast %struct.rtx_def* (%struct.rtx_def*, ...)* %33 to i8*
	// %35 = ptrtoint %struct.rtx_def* (%struct.rtx_def*, ...)** %32 to i64
  // %36 = call i8* @__autc(i8* %34, i64 %35, i64 3088)
  // %37 = call i8* @__tagc_num(i8* %36, i64 1, i64 3089)
	// ...
	// call void @__echk(i8* %41, i64 2, i64 29817)
	// %42 = call i8* @__autc(i8* %41, i64 2, i64 29818)
  if (fty->isVarArg()) {
		//errs() << "fty: "; fty->dump();
		bool found = false;
		for (auto pU: pLI->users()) {
			if (auto pBC = dyn_cast<BitCastInst>(pU)) {
				if (pBC->getDestTy() == Type::getInt8PtrTy(*C)) {
					//errs() << "pBC: "; pBC->dump();
					for (auto pU2: pBC->users()) {
						if (auto pCB2 = dyn_cast<CallBase>(pU2)) {
							//errs() << "pCB2: "; pCB2->dump();
							auto pF2 = pCB2->getCalledFunction();
							if (pF2 && pF2->getName().find("__echk") != std::string::npos) {
								context = dyn_cast<ConstantInt>(pCB2->getOperand(1));
								found = true;
								//errs() << "Found! context: "; context->dump();
								break;
							}
						}
					}

					if (found)
						break;
				}
			}
		}
	}

  // MAC disclosure sequence
  // val_op = load ptr_op
  // autc val_op, ptr_op
  // tagc val_op, context
	IRBuilder<> Builder(pLI->getNextNode());
	auto castA = Builder.CreateCast(Instruction::BitCast, val_op, Type::getInt8PtrTy(*C));
	auto castB = Builder.CreatePtrToInt(ptr_op, Type::getInt64Ty(*C));

	Value *callB = nullptr;

	if (llvm::ACFI::getAcfiInstType() == AcfiType::ACFI_MC) {
		callB = insertTagc(M, &Builder, castA, context);
	} else {
		auto callA = insertAutc(M, &Builder, castA, castB);
		callB = insertTagc(M, &Builder, callA, context);
	}

	auto castC = Builder.CreateCast(Instruction::BitCast, callB, val_op->getType());

	pLI->replaceAllUsesWith(castC);
	dyn_cast<Instruction>(castA)->setOperand(0, val_op);
}

void MCPass::handleAllocaInst(Module &M, AllocaInst *pAI) {
	Type *ty = pAI->getAllocatedType();
  auto size_in_bits = pAI->getAllocationSizeInBits(*DL);

	//if (size_in_bits == llvm::None)
	//	return;

  // Check Return Inst
	bool chk = false;
	auto pF = pAI->getFunction();
	Instruction *pIB = nullptr;

	for (auto &BB: *pF) {
		for (auto &I: BB) {
			if (dyn_cast<ReturnInst>(&I)) {
				pIB = &I;
				chk = true;
				break;
			}
		}
	}

  ASSERT(chk); //TODO check
	if (!chk)
		return;

	if (dyn_cast<UnreachableInst>(pIB)) //TODO check
		return;

	// If pAI is a function pointer, just insert bclrm
	auto pty = dyn_cast<PointerType>(ty);
	if (pty && pty->getPointerElementType()->isFunctionTy()) {
		FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C)}, false);
		IRBuilder<> Builder(pIB);
		auto castA = Builder.CreateCast(Instruction::BitCast, pAI, Type::getInt8PtrTy(*C));
		insertBclrm(M, &Builder, castA);
	} else {
		//FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
		FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);
		Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
		Value *size = ConstantInt::get(Type::getInt64Ty(*C), (*size_in_bits) / 8);
		//auto clear = M.getOrInsertFunction("__clear_wpb", FuncTypeA);
		auto clear = M.getOrInsertFunction("__clear_wpb_num", FuncTypeA);

		IRBuilder<> Builder(pIB);
		auto castA = Builder.CreateCast(Instruction::BitCast, pAI, Type::getInt8PtrTy(*C));
		Builder.CreateCall(clear, {castA, size, num});
	}
}

bool MCPass::hasFuncPtrTy(Type* ty) {
  // Pointer type
  if (auto pty = dyn_cast<PointerType>(ty))
    return pty->getPointerElementType()->isFunctionTy();

  // Array type
  if (ty->isArrayTy()) {
    while (ty->isArrayTy())
      ty = ty->getArrayElementType();

    // Check element type
    if (auto pty = dyn_cast<PointerType>(ty)) {
      if (pty->getPointerElementType()->isFunctionTy())
        return true;
    } else if (auto sty = dyn_cast<StructType>(ty)) {
      for (auto it = sty->element_begin(); it != sty->element_end(); it++) {
        if (hasFuncPtrTy(*it))
          return true;
      }
    }

    return false;
  }

  // Struct type
  if (auto sty = dyn_cast<StructType>(ty)) {
    for (auto it = sty->element_begin(); it != sty->element_end(); it++) {
      if (hasFuncPtrTy(*it))
        return true;
    }

    return false;
  }

  return false;
}

void MCPass::handleWrapperFunctions(Module &M) {
  Function *memset = nullptr;
  Function *memcpy = nullptr;
  Function *memmove = nullptr;
  Function *sigsetjmp = nullptr;
  Function *setjmp = nullptr;
  Function *siglongjmp = nullptr;
  Function *longjmp = nullptr;

  for (auto &F: M) {
    if (F.getName() == "llvm.memset.p0i8.i64")
      memset = &F;
    else if (F.getName() == "llvm.memcpy.p0i8.p0i8.i64")
      memcpy = &F;
    else if (F.getName() == "llvm.memmove.p0i8.p0i8.i64")
      memmove = &F;
    else if (F.getName() == "__sigsetjmp")
      sigsetjmp = &F;
    else if (F.getName() == "_setjmp")
      setjmp = &F;
    else if (F.getName() == "siglongjmp")
      siglongjmp = &F;
    else if (F.getName() == "longjmp")
      longjmp = &F;
  }

  if (memset) {
    FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt8Ty(*C), Type::getInt64Ty(*C), Type::getInt1Ty(*C)}, false);
    auto wrapper = M.getOrInsertFunction("__memset_wrapper", FuncTypeA);
		auto callee = wrapper.getCallee();
    memset->replaceAllUsesWith(callee);
  }

  if (memcpy) {
    FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt1Ty(*C)}, false);
    auto wrapper = M.getOrInsertFunction("__memcpy_wrapper", FuncTypeA);
		auto callee = wrapper.getCallee();
    memcpy->replaceAllUsesWith(callee);
  }

  if (memmove) {
		FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt1Ty(*C)}, false);
    auto wrapper = M.getOrInsertFunction("__memmove_wrapper", FuncTypeA);
		auto callee = wrapper.getCallee();
    memmove->replaceAllUsesWith(callee);
  }

  //if (sigsetjmp) {
  //  FunctionType *FuncTypeA = FunctionType::get(Type::getInt32Ty(*C), {pI->getOperand(0)->getType()}, false);
  //  auto wrapper = M.getOrInsertFunction("__setjmp_cpt", FuncTypeA);
	//	auto callee = wrapper.getCallee();
  //  sigsetjmp->replaceAllUsesWith(callee);
  //}

  //if (setjmp) {
  //  FunctionType *FuncTypeA = FunctionType::get(Type::getInt32Ty(*C), {pI->getOperand(0)->getType()}, false);
  //  auto wrapper = M.getOrInsertFunction("__setjmp_cpt", FuncTypeA);
	//	auto callee = wrapper.getCallee();
  //  setjmp->replaceAllUsesWith(callee);
  //}

  //if (siglongjmp) {
  //  FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {pI->getOperand(0)->getType(), Type::getInt32Ty(*C)}, false);
  //  auto wrapper = M.getOrInsertFunction("__longjmp_wrapper", FuncTypeA);
	//	auto callee = wrapper.getCallee();
  //  siglongjmp->replaceAllUsesWith(callee);
  //}

  //if (longjmp) {
  //  FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {pI->getOperand(0)->getType(), Type::getInt32Ty(*C)}, false);
  //  auto wrapper = M.getOrInsertFunction("__longjmp_wrapper", FuncTypeA);
	//	auto callee = wrapper.getCallee();
  //  longjmp->replaceAllUsesWith(callee);
  //}

  // realloc will be handled using hook
}

Value *MCPass::insertTagc(Module &M, IRBuilder<> *Builder, Value *pV, Value *context) {
  //FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
  FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);
  Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
	Value *castA = nullptr;

	if (pV->getType()->isPointerTy())
		castA = Builder->CreateCast(Instruction::BitCast, pV, Type::getInt8PtrTy(*C));
	else
		castA = Builder->CreateIntToPtr(pV, Type::getInt8PtrTy(*C));

  //auto tagc = QEMU? M.getOrInsertFunction("__tagc", FuncTypeA) :
  auto tagc = QEMU? M.getOrInsertFunction("__tagc_num", FuncTypeA) :
                    Intrinsic::getDeclaration(&M, Intrinsic::pa_tagc, {Type::getInt8PtrTy(*C)});

  //return Builder->CreateCall(tagc, {castA, context}, "");
  return Builder->CreateCall(tagc, {castA, context, num}, "");
}

Value *MCPass::insertAutc(Module &M, IRBuilder<> *Builder, Value *pV, Value *context) {
  FunctionType *FuncTypeA = FunctionType::get(Type::getInt8PtrTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C), Type::getInt64Ty(*C)}, false);
  Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);

  if (QEMU) {
    auto autc = M.getOrInsertFunction("__autc", FuncTypeA);
    return Builder->CreateCall(autc, {pV, context, num}, "");
  } else {
    auto autc = Intrinsic::getDeclaration(&M, Intrinsic::pa_autc, {Type::getInt8PtrTy(*C)});
    return Builder->CreateCall(autc, {pV, context}, "");
  }
}

void MCPass::insertEstr(Module &M, IRBuilder<> *Builder, Value *pV, Value *context) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
	auto estr = QEMU? M.getOrInsertFunction("__estr", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::acfi_estr);
	Builder->CreateCall(estr, {pV, context}, "");
}

void MCPass::insertEact(Module &M, IRBuilder<> *Builder, Value *pV, Value *context) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
	auto eact = QEMU? M.getOrInsertFunction("__eact", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::acfi_eact);
	Builder->CreateCall(eact, {pV, context}, "");
}

void MCPass::insertEchk(Module &M, IRBuilder<> *Builder, Value *pV, Value *context) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
	auto eact = QEMU? M.getOrInsertFunction("__echk", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::acfi_echk);
	Builder->CreateCall(eact, {pV, context}, "");
}

void MCPass::insertBsetm(Module &M, IRBuilder<> *Builder, Value *pV) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C)}, false);
	auto bsetm = QEMU? M.getOrInsertFunction("__bsetm", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::bm_bsetm);
	Builder->CreateCall(bsetm, {pV}, "");
}

void MCPass::insertBclrm(Module &M, IRBuilder<> *Builder, Value *pV) {
	FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C)}, false);
	auto bclrm = QEMU? M.getOrInsertFunction("__bclrm", FuncTypeA) :
										Intrinsic::getDeclaration(&M, Intrinsic::bm_bclrm);
	Builder->CreateCall(bclrm, {pV}, "");
}

void MCPass::init(Module &M) {
	for (auto &F : M) {
		if (&F && F.getName() == "main") {
		  C = &F.getContext();
			DL = &F.getParent()->getDataLayout();
			main = &F;
			break;
		}
	}
}
