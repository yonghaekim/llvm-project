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
#include "llvm/DPT/DPT.h"
#include "llvm/Support/CommandLine.h"
#include <regex>
#include <map>
#include <iostream>

using namespace llvm;
using namespace DPT;
using namespace std;

//#define DEBUG_TYPE "aos_reach_test_pass"

//namespace {
  class DataPtrTagPass : public ModulePass {

  public:
    static char ID; // Pass identification, replacement for typeid
    DataPtrTagPass() : ModulePass(ID) {}

    set<GetElementPtrInst *> gep_visit_set;
    set<GetElementPtrInst *> gep_erase_set;

		LLVMContext *C = NULL;
		const DataLayout *DL = NULL;
		Function *main = nullptr;
		Function *entry = nullptr;
    char *voidptr = nullptr;

    bool runOnModule(Module &M) override;
		//void getAnalysisUsage(AnalysisUsage &AU) const;

    bool BASE = false;
    bool DPT_H = false;
    bool DPT_C = false;
    bool DPT_F = false;
		bool QEMU = false;

    unsigned statNumGV = 0;
    unsigned statNumAI = 0;
    unsigned statNumCI = 0;
    unsigned statNumGEP = 0;

    unsigned temp_cnt = 0;
    unsigned intr_num = 0;
    unsigned func_num = 0;
    unsigned tagd_cnt = 0;

  private:  
		void handleGlobalVariables(Module &M);
		void handleGlobalVariable(Module &M, GlobalVariable *pGV);
		void handleAllocaInsts(Module &M);
		void handleAlloca(Module &M, AllocaInst *pAI);
    void insertCclrChains(Module &M, AllocaInst *pAI, Instruction *pTI, Instruction *pRI);
    void handleStructType(Module &M, AllocaInst *pAI, Instruction *pTI, Instruction *pRI, StructType *sty, vector<Value*> *indices);
    void handleStructArrayType(Module &M, AllocaInst *pAI, Instruction *pTI, Instruction *pRI, ArrayType *aty, vector<Value*> *indices);
    void handleArrayType(Module &M, AllocaInst *pAI, Instruction *pTI, Instruction *pRI, ArrayType *aty, vector<Value*> *indices);
		void handleMallocs(Module &M);
		void handleMalloc(Module &M, CallBase *pCB);
		void handleFree(Module &M, Instruction *pI);

		void handleGEPs(Module &M);
		void handleGEP(Module &M, GetElementPtrInst *pGEP);
    void handlePtrToInts(Module &M);
		void handleQEMU(Module &M);
    void addXtag(Instruction *pI, unsigned idx, Module &M);

    //Instruction *insertTagd(Module &M, IRBuilder<> *Builder, Value *pV);
    Instruction *insertTagd(Module &M, IRBuilder<> *Builder, Value *pV, Value *context);
    Instruction *insertXtag(Module &M, IRBuilder<> *Builder, Value *pV);
    Instruction *insertCstr(Module &M, IRBuilder<> *Builder, Value *pV, Value *arg);
    Instruction *insertCclr(Module &M, IRBuilder<> *Builder, Value *pV);
    Instruction *insertBsetm(Module &M, IRBuilder<> *Builder, Value *pV);
    Instruction *insertBclrm(Module &M, IRBuilder<> *Builder, Value *pV);
    Instruction *insertBextm(Module &M, IRBuilder<> *Builder, Value *pV);

		bool handleInstructions(Module &M);
		bool handleDealloc(Function *pF, ReturnInst *pRI);
		bool handleMalloc(Function *pF, Instruction *pI);
    bool IsStructTyWithArray(Type *ty);
		void init(Module &M);
    void insertDptSet(Module &M);
    void handleIntrinsicFunctions(Module &M);
    Function *createWrapperFunction(Module &M, Function &F, bool replace_use);
    void addXpacm(Instruction *pI, unsigned idx, Module &M);
    void printFuncAddr(Module &M);
    void handleWrapperFunctions(Module &M);

    set<Value*> gvop_set;
    void findOperators(Module &M, Value *pV);
		map<Value*,Value*> user_map;
    Value *createOperator(Module &M, GlobalVariable *pGV, Value *pOp, Instruction *pTI);
  };
//}

