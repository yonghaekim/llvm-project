#include "llvm/ACFI/IFCCPass.h"
#include "llvm/ACFI/ACFI.h"
#include "llvm/IR/InlineAsm.h"

#define ASSERT(x) if (!(x)) *(voidptr) = 0;

char IFCCPass::ID = 0;
static RegisterPass<IFCCPass> X("ifcc", "IFCC pass");

Pass *llvm::ACFI::createIFCCPass() { return new IFCCPass(); }

bool IFCCPass::runOnModule(Module &M) {
	errs() << "Start IFCC pass!\n";

	init(M);
  auto const acfiInstType = llvm::ACFI::getAcfiInstType();
  auto const acfiQemuMode = llvm::ACFI::getAcfiQemuMode();

  //if (acfiInstType == AcfiType::None)
	//  return false;

	//printFuncNum(M);

	QEMU = (acfiQemuMode == AcfiQemuEn::Enable);
  LARGE_ALIGN = true;

  if (acfiInstType == AcfiType::IFCC) {
		findWhiteSet(M);
		createJumpTables(M);
		replaceUsers(M);
		createJumpTableEntries(M);
		handleIndirectCalls(M);
	}

	if (QEMU) {
	  printFuncAddr(M);
	}

	errs() << "statMaxEqClassSize: " << statMaxEqClassSize << "\n";
	errs() << "statNumIndirectCall: " << statNumIndirectCall << "\n";

	return true; // function_modified = true
}

void IFCCPass::findWhiteSet(Module &M) {
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
			}
		}
	}

	//errs() << "Size of white set: " << white_set.size() << "\n";
	//for (auto pF: white_set) {
	//	errs() << "--" << pF->getName() << "\n";
	//}
}

void IFCCPass::createJumpTables(Module &M) {
	for (auto pF: white_set) {
		if (pF->isVarArg())
			errs() << "Found VarArg! " << pF->getName() << "\n";

		unsigned num_param = pF->getFunctionType()->getNumParams();

		if (!fl_map[num_param])
			fl_map[num_param] = new list<Function*>;

		fl_map[num_param]->push_back(pF);

		if (!jt_map[num_param]) {
			FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), false);
			Function *jt = Function::Create(FuncTypeA, Function::ExternalLinkage, "__cfi_jumptable_" + to_string(num_param), M);
			//jt->setAttributes(main->getAttributes());
			//jt->removeRetAttr(Attribute::SExt);
			jt->addFnAttr(Attribute::NoUnwind);
			jt->addFnAttr(Attribute::NoInline);
			jt->addFnAttr(Attribute::OptimizeNone);
			jt->setVisibility(GlobalVariable::HiddenVisibility);
			auto pBB = BasicBlock::Create(*C, "", jt, nullptr);
			new UnreachableInst(*C, pBB);
			jt->setAlignment(Align(8));
			jt_map[num_param] = jt;
		}
	}

  if (LARGE_ALIGN) {
    // Set alignment
    for (auto x: fl_map) {
      unsigned num_param = x.first;

      if (auto func_list = x.second) {
        unsigned num_entry = func_list->size();
        //unsigned pow2cnt = getNextPow2(16*num_entry);
        unsigned pow2cnt = getNextPow2(12*num_entry);

        jt_map[num_param]->setAlignment(Align(pow2cnt));
        errs() << "Set alignment: " << pow2cnt << "\n";
      }
    }
  }
}

void IFCCPass::replaceUsers(Module &M) {
	for (auto x: jt_map) {
		unsigned num_param = x.first;
		Function *jt = x.second;

		if (!jt)
			continue;

		unsigned offset = 0;
	  auto context = ConstantInt::get(Type::getInt64Ty(*C), num_param);

		for (auto pF: *fl_map[num_param]) {
			Value *jte = nullptr;

			// CreateGEP
			auto castA = ConstantExpr::getPtrToInt(jt, Type::getInt64Ty(*C));
			//auto offsetA = ConstantInt::get(Type::getInt64Ty(*C), offset*16);
			auto offsetA = ConstantInt::get(Type::getInt64Ty(*C), offset*12);
			auto addA = ConstantExpr::getAdd(castA, offsetA);
			//jte = ConstantExpr::getIntToPtr(addA, Type::getInt8PtrTy(*C));
			jte = ConstantExpr::getIntToPtr(addA, pF->getType());
			offset++;

			// Replace uses of pF with jte
			// But, shouldn't touch direct calls
			// TODO: could implement this using replaceUsesWithIf?
			set<CallBase*> call_set;
			findDirectCalls(M, pF, call_set);

			//errs() << "pF: " << pF->getName() << "\n";

			// Replace func addr with jte
			pF->replaceAllUsesWith(jte);

			for (auto pCB: call_set) {
				//errs() << "isIndirectCall(): " << pCB->isIndirectCall() << "\n";
				//errs() << "isTailCall(): " << pCB->isTailCall() << "\n";
				//errs() << "isInlineAsm(): " << pCB->isInlineAsm() << "\n";
				auto pV = pCB->getCalledOperand();
				auto pBC = ConstantExpr::getBitCast(pF, pV->getType());
				pCB->setCalledOperand(pBC);
			}

			for (auto pU: jte->users()) {
				if (auto pGA = dyn_cast<GlobalAlias>(pU)) {
					auto pV = pGA->getAliasee();
					auto pBC = ConstantExpr::getBitCast(pF, pV->getType());
					pGA->setAliasee(pBC);
				}
			}
		}
	}
}

void IFCCPass::findDirectCalls(Module &M, Value *pV, set<CallBase*> &call_set) {
	for (auto pU: pV->users()) {
		//errs() << "pU: "; pU->dump();

		if (auto pCB = dyn_cast<CallBase>(pU)) {
			if (pCB->getCalledOperand() == pV) {
				//errs() << "Insert(1): "; pCB->dump();
				call_set.insert(pCB);
			}
		} else if (auto pGA = dyn_cast<GlobalAlias>(pU)) {
			//errs() << "pGA: "; pU->dump();
			findDirectCalls(M, pGA, call_set);
		} else if (dyn_cast<Instruction>(pU)) {
		} else if (auto pOP = dyn_cast<Operator>(pU)) {
			findDirectCalls(M, pOP, call_set);
		}
	}
}

void IFCCPass::createJumpTableEntries(Module &M) {
	// Make jump table entries
	// We make those after replacing the uses of functions
	for (auto x: jt_map) {
		unsigned num_param = x.first;

		if (Function *jt = x.second) {
      unsigned cnt = 0;
      auto &I = jt->back().back();
      IRBuilder<> Builder(&I);

			errs() << "# params(" << num_param << ") " << jt->getName() << "\n";

			FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), {Type::getInt8PtrTy(*C)}, false);
			InlineAsm *jalr = InlineAsm::get(FuncTypeA, "jalr $0, 0($0)\0A", "r", /*hasSideEffects=*/ true);

			for (auto pF: *fl_map[num_param]) {
				auto castA = Builder.CreateCast(Instruction::BitCast, pF, Type::getInt8PtrTy(*C));
			  Builder.CreateCall(jalr, {castA});

				//FunctionType *FuncTypeB = FunctionType::get(Type::getVoidTy(*C), {}, false);
			  //InlineAsm *trap = InlineAsm::get(FuncTypeB, "ebreak", "", /*hasSideEffects=*/ true);
        //Builder.CreateCall(trap);
        //Builder.CreateCall(trap);
        cnt++;

				errs() << "  -- " << pF->getName() << "\n";
			}

			if (cnt > statMaxEqClassSize)
				statMaxEqClassSize = cnt;

      //unsigned pow2cnt = getNextPow2(16*cnt);
			//unsigned num_pad = (pow2cnt - 16*cnt) / 2;
      unsigned pow2cnt = getNextPow2(12*cnt);
			unsigned num_pad = (pow2cnt - 12*cnt) / 2;
			//errs() << "cnt: " << cnt << "\n";
			//errs() << "pow2cnt: " << pow2cnt << "\n";
			//errs() << "num_pad: " << num_pad << "\n";

      //errs() << "cnt: " << cnt << " pow2cnt: " << pow2cnt << " num_pad: " << num_pad << "\n";
			FunctionType *FuncTypeB = FunctionType::get(Type::getVoidTy(*C), {}, false);
			InlineAsm *trap = InlineAsm::get(FuncTypeB, "ebreak", "", /*hasSideEffects=*/ true);

      for (unsigned i=0; i<num_pad; i++) {
				//errs() << "i: " << i << "\n";
        Builder.CreateCall(trap);
      }
		}
	}
}

void IFCCPass::handleIndirectCalls(Module &M) {
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

void IFCCPass::handleIndirectCall(Module &M, CallBase *pCB) {
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

	//unsigned num_param = fty0->getNumParams();
	unsigned num_param = 0;

	for (auto itr = pCB->arg_begin() ; itr != pCB->arg_end(); itr++) {
		num_param++;
	}
	//unsigned num_param = pCB->getNumOperands() - 1;

	//errs() << "pCB: "; pCB->dump();
	//errs() << "fty0: "; fty0->dump();
	//errs() << "num_param: " << num_param << "\n";
	//errs() << "numOperands: " << pCB->getNumOperands() << "\n";

	auto context = ConstantInt::get(Type::getInt64Ty(*C), num_param);
	Function *jt = jt_map[num_param];

  if (!jt) {
    // Create a fake jumptable
		// The access to this will end up failing
		if (!fl_map[num_param])
			fl_map[num_param] = new list<Function*>;

    errs() << "Jumptable is not found!\n";
    FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(*C), false);
    jt = Function::Create(FuncTypeA, Function::ExternalLinkage, "__cfi_jumptable", M);
    auto pBB = BasicBlock::Create(*C, "", jt, nullptr);
    new UnreachableInst(*C, pBB);
    jt->setAlignment(Align(8));

    jt_map[num_param] = jt;

    IRBuilder<> Builder(&(jt->back().back()));
    for (unsigned i=0; i<8; i++) {
      FunctionType *FuncTypeB = FunctionType::get(Type::getVoidTy(*C), {}, false);
      InlineAsm *trap = InlineAsm::get(FuncTypeB, "ebreak", "", /*hasSideEffects=*/ true);
      Builder.CreateCall(trap);
    }
  }

	//unsigned mask = fl_map[num_param]->size() * 16;
	unsigned mask = fl_map[num_param]->size() * 12;
	mask = getNextPow2(mask);
	mask--;
	//mask &= ~0xF;
	//mask &= ~0x7;

	// 1) jte = jt + (jte & mask);
	// 2) jte = jt + (callee - jt) & mask;
	IRBuilder<> Builder(pCB);
	Value *jte = nullptr;

	//if (LARGE_ALIGN) {
		auto addr = Builder.CreatePtrToInt(callee, Type::getInt64Ty(*C));
		auto cnst = ConstantInt::get(Type::getInt64Ty(*C), mask);
		auto offset = Builder.CreateAnd(addr, cnst);
		auto base = Builder.CreatePtrToInt(jt, Type::getInt64Ty(*C));
		auto res = Builder.CreateAdd(base, offset);
		jte = Builder.CreateIntToPtr(res, callee->getType());
		pCB->setCalledOperand(jte);
		//replaceOp(callee, jte, pCB);
	//} else {
	//	auto op1 = Builder.CreatePtrToInt(callee, Type::getInt64Ty(*C));
	//	auto op2 = Builder.CreatePtrToInt(jt, Type::getInt64Ty(*C));
	//	auto res1 = Builder.CreateSub(op1, op2);
	//	auto cnst = ConstantInt::get(Type::getInt64Ty(*C), mask);
	//	auto offset = Builder.CreateAnd(res1, cnst);
	//	auto res2 = Builder.CreateAdd(op2, offset);
	//	jte = Builder.CreateIntToPtr(res2, callee->getType());
	//	//errs() << "Before: "; pCB->dump();
	//	pCB->setCalledOperand(jte);
	//	//errs() << "After: "; pCB->dump();
	//	//replaceOp(callee, jte, pCB);
	//}

	if (true) {
	//if (false) {
		// This is for debugging
		//FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
		FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C), Type::getInt64Ty(*C)}, false);
		Value *num = ConstantInt::get(Type::getInt64Ty(*C), temp_cnt++);
		//auto print = M.getOrInsertFunction("__print_value", FuncTypeA);
		auto check = M.getOrInsertFunction("__check_value", FuncTypeA);

		//Builder.CreateCall(print, {castA, num}); //
		//Builder.CreateCall(print, {callA, num}); //
		auto castA = Builder.CreateCast(Instruction::BitCast, callee, Type::getInt8PtrTy(*C));
		//Builder.CreateCall(print, {castA, num}); //
		//auto castB = Builder.CreateCast(Instruction::BitCast, jt, Type::getInt8PtrTy(*C));
		//Builder.CreateCall(print, {castB, num}); //
		auto castC = Builder.CreateCast(Instruction::BitCast, jte, Type::getInt8PtrTy(*C));
		Builder.CreateCall(check, {castA, castC, num}); //
	}
}

void IFCCPass::printFuncAddr(Module &M) {
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

  for (auto x: jt_map) {
		unsigned num_param = x.first;
		Function *jt = x.second;

		if (!jt)
			continue;

    Constant *name = ConstantDataArray::getString(*C, jt->getName(), true);

    GlobalVariable* pGV = new GlobalVariable(M, 
      /*Type=*/ name->getType(),
      /*isConstant=*/ true,
      /*Linkage=*/ GlobalValue::PrivateLinkage,
      /*Initializer=*/ 0, // has initializer, specified below
      /*Name=*/ ".func_name");
    pGV->setAlignment(Align(1));
    pGV->setInitializer(name);

    auto castA = Builder.CreateCast(Instruction::BitCast, pGV, Type::getInt8PtrTy(*C));
    auto castB = Builder.CreateCast(Instruction::BitCast, jt, Type::getInt8PtrTy(*C));
    FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C)}, false);
    auto print = M.getOrInsertFunction("__print_func", FuncTypeA);
    Builder.CreateCall(print, {castA, castB});

		unsigned offset = 0;

		for (auto pF: *fl_map[num_param]) {
			// CreateGEP
			//auto idx = ConstantInt::get(Type::getInt64Ty(*C), offset*4);
			//auto cast = ConstantExpr::getBitCast(jt, PointerType::get(Type::getInt8PtrTy(*C), 0));
			//auto gep = ConstantExpr::getGetElementPtr(Type::getInt8PtrTy(*C), cast, idx);
			//auto jte = ConstantExpr::getBitCast(gep, Type::getInt8PtrTy(*C));
      auto castA = ConstantExpr::getPtrToInt(jt, Type::getInt64Ty(*C));
			//auto offsetA = ConstantInt::get(Type::getInt64Ty(*C), offset*16);
			auto offsetA = ConstantInt::get(Type::getInt64Ty(*C), offset*12);
      auto addA = ConstantExpr::getAdd(castA, offsetA);
      auto jte = ConstantExpr::getIntToPtr(addA, Type::getInt8PtrTy(*C));
			offset++;

      Constant *name = ConstantDataArray::getString(*C, pF->getName(), true);

      GlobalVariable* pGV = new GlobalVariable(M, 
        /*Type=*/ name->getType(),
        /*isConstant=*/ true,
        /*Linkage=*/ GlobalValue::PrivateLinkage,
        /*Initializer=*/ 0, // has initializer, specified below
        /*Name=*/ ".func_name");
      pGV->setAlignment(Align(1));
      pGV->setInitializer(name);

    {
      auto castA = Builder.CreateCast(Instruction::BitCast, pGV, Type::getInt8PtrTy(*C));
      auto castB = Builder.CreateCast(Instruction::BitCast, pF, Type::getInt8PtrTy(*C));
      FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C)}, false);
      auto print = M.getOrInsertFunction("__print_func", FuncTypeA);
      Builder.CreateCall(print, {castA, castB});
    }
    {
      auto castA = Builder.CreateCast(Instruction::BitCast, pGV, Type::getInt8PtrTy(*C));
      auto castB = Builder.CreateCast(Instruction::BitCast, jte, Type::getInt8PtrTy(*C));
      FunctionType *FuncTypeA = FunctionType::get(Type::getVoidTy(M.getContext()), {Type::getInt8PtrTy(*C), Type::getInt8PtrTy(*C)}, false);
      auto print = M.getOrInsertFunction("__print_func", FuncTypeA);
      Builder.CreateCall(print, {castA, castB});
    }
    }
  }
}

void IFCCPass::printFuncNum(Module &M) {
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

void IFCCPass::init(Module &M) {
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

