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
  class ShadowCallMemPass : public ModulePass {

  public:
    static char ID; // Pass identification, replacement for typeid
    ShadowCallMemPass() : ModulePass(ID) {}

		list<Function *> func_list;
		list<Function *> uncalled_list;
    map<Instruction *, list<list<Operator *>>> inst_map;

    bool runOnModule(Module &M) override;
		//void getAnalysisUsage(AnalysisUsage &AU) const;
    //ShadowCallMemPass::CptNode* getRootNode();
    //map<Value *, ShadowCallMemPass::CptNode *> getValueMap();
    list<Function *>  getUncalledList();

    bool IFCC = false;
    bool PA = false;
    bool ACFI = false;
		bool QEMU = false;
    bool Relaxed = false;
    char *voidptr = nullptr;
		LLVMContext *C;
		const DataLayout *DL;
    Function *main = nullptr;
    Function *entry = nullptr;
    unsigned temp_num = 0;

  private:  
    void handleInstructions(Module &M);
    void handleStoreInst(Module &M, StoreInst *pSI);
    void handleLoadInst(Module &M, LoadInst *pLI);
    void handleAllocaInst(Module &M, AllocaInst *pAI);
    void handleFree(Module &M, Instruction *pI);
    bool hasFuncPtrTy(Type* ty);
    void handleWrapperFunctions(Module &M);
    void handleMemSetInst(Module &M, Instruction *pI);
    void handleMemCpyInst(Module &M, Instruction *pI);
    void handleMemMoveInst(Module &M, Instruction *pI);
    void handleReallocInst(Module &M, Instruction *pI);
    void handleSetJmpInst(Module &M, Instruction *pI);
    void handleLongJmpInst(Module &M, Instruction *pI);
    void replaceUser(Module &M, Value *pVa, Value *pVb, Instruction *pI);
    void replaceUserGv(Module &M, Value *pVa, Value *pVb, GlobalVariable *pGV);
    void printStoreValues(Module &M);
    Value *insertTagi(Module &M, IRBuilder<> *Builder, Value *pV, Value *pF);
    Value *insertTagd(Module &M, IRBuilder<> *Builder, Value *pV, Value *pF);
    Instruction *insertAuti(Module &M, IRBuilder<> *Builder, Value *pV, Value *pF);
    void insertBsetm(Module &M, IRBuilder<> *Builder, Value *pV);
    Value *insertClosure(Module &M, IRBuilder<> *Builder, Value *pVa, Value *pVb, Value *context);
    Value *insertDisclosure(Module &M, IRBuilder<> *Builder, Value *pVa, Value *pVb, Value *context);
    void handleBitCastInsts(Module &M);
		void init(Module &M);
  };
//}

