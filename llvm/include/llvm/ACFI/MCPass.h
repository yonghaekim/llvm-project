#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/Pass.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/ACFI/ACFI.h"
#include "llvm/Support/CommandLine.h"
#include <regex>
#include <map>
#include <iostream>

using namespace llvm;
using namespace ACFI;
using namespace std;

//namespace {
  class MCPass : public ModulePass {

  public:
    static char ID; // Pass identification, replacement for typeid
    MCPass() : ModulePass(ID) {}
    bool runOnModule(Module &M) override;
    void getAnalysisUsage(AnalysisUsage &AU) const;

  private:  
		bool QEMU = false;
    char *voidptr = nullptr;
		LLVMContext *C = nullptr;
		const DataLayout *DL = nullptr;
    Function *main = nullptr;
    Function *init_acfi = nullptr;
    unsigned int temp_cnt = 0;
    unsigned int func_num = 0;
    unsigned int call_num = 0;

    Value *insertTagc(Module &M, IRBuilder<> *Builder, Value *jte, Value *context);
    Value *insertAutc(Module &M, IRBuilder<> *Builder, Value *pV, Value *context);
    void insertEstr(Module &M, IRBuilder<> *Builder, Value *pV, Value *context);
    void insertEact(Module &M, IRBuilder<> *Builder, Value *pV, Value *context);
    void insertEchk(Module &M, IRBuilder<> *Builder, Value *pV, Value *context);
    void insertBsetm(Module &M, IRBuilder<> *Builder, Value *pV);
    void insertBclrm(Module &M, IRBuilder<> *Builder, Value *pV);

    void handleInstructions(Module &M);
    void handleStoreInst(Module &M, StoreInst *pSI);
    void handleLoadInst(Module &M, LoadInst *pLI);
    void handleAllocaInst(Module &M, AllocaInst *pAI);
    bool hasFuncPtrTy(Type* ty);
    void handleWrapperFunctions(Module &M);

		void init(Module &M);
  };
//}
