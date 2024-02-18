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
  class ACFIPass : public ModulePass {

  public:
    static char ID; // Pass identification, replacement for typeid
    ACFIPass() : ModulePass(ID) {}
    bool runOnModule(Module &M) override;

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

  	set<Function*> white_set;

    void findWhiteSet(Module &M);
    void replaceUsers(Module &M);
    void findUsers(Module &M, Value *pV, map<Value*,set<Value*>*> &user_map);
    void handlePHINode(Module &M, PHINode *pPN, Value *pV, Value *context);
    void handleInstruction(Module &M, Instruction *pI, Value *pV, Value *context);

    void handleGlobalVariables(Module &M);
    void handleGlobalVariable(Module &M, GlobalVariable *pGV);
    void getIndices(Module &M, GlobalVariable *pGV, Constant *pConst, vector<Value*> *indices);
    void handleFunctionOrOperator(Module &M, GlobalVariable *pGV, Constant *pC, vector<Value*> *indices);
    void overwriteFuncAddr(Module &M, GlobalVariable *pGV, Function *pF, Value *pV, vector<Value*> *indices);
    Value *insertTagc(Module &M, IRBuilder<> *Builder, Value *jte, Value *context);
    Value *insertAutc(Module &M, IRBuilder<> *Builder, Value *pV, Value *context);
    void insertEstr(Module &M, IRBuilder<> *Builder, Value *pV, Value *context);
    void insertEact(Module &M, IRBuilder<> *Builder, Value *pV, Value *context);
    void insertEchk(Module &M, IRBuilder<> *Builder, Value *pV, Value *context);
    void insertBsetm(Module &M, IRBuilder<> *Builder, Value *pV);
    void initEMT(Module &M);

    void handleIntToPtrInsts(Module &M);
    void handleIndirectCalls(Module &M);
    void handleIndirectCall(Module &M, CallBase *pCB);


    void printFuncAddr(Module &M);
    void printFuncNum(Module &M);
		void init(Module &M);

    unsigned statNumESTR = 0;
    unsigned statMaxEqClassSize = 0;
    unsigned statNumIndirectCall = 0;
  };
//}
