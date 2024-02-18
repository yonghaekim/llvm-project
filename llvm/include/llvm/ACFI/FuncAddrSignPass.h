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
#include "llvm/ACFI/JumpTableGenPass.h"

using namespace llvm;
using namespace ACFI;
using namespace std;

//namespace {
  class FuncAddrSignPass : public ModulePass {

  public:
    static char ID; // Pass identification, replacement for typeid
    FuncAddrSignPass() : ModulePass(ID) {}
    bool runOnModule(Module &M) override;
    void getAnalysisUsage(AnalysisUsage &AU) const;

    bool LARGE_ALIGN = false;
    bool IFCC = false;
    bool PA = false;
    bool ACFI = false;
		bool QEMU = false;
  	set<Function*> white_set;
    map<unsigned, list<Function*>*> fl_map;
    map<unsigned, Function*> jt_map;

  	set<Value*> visit_set;

  private:  
    char *voidptr = nullptr;
		LLVMContext *C = nullptr;
		const DataLayout *DL = nullptr;
		Function *main = nullptr;
		Function *entry = nullptr;
    unsigned temp_cnt = 0;
    std::map<const Type *, uint64_t> TypeIDCache;

    void replaceUsers(Module &M);
    void replaceUser(Module &M, Instruction *pI, Value *pV, Value *jte, Value *context);
    void handleBitCastOperator(Module &M, BitCastOperator *pBC, Value *jte, Value *context);
    void handlePtrToIntOperator(Module &M, PtrToIntOperator *pPTI, Value *jte, Value *context);
    void handlePHINode(Module &M, PHINode *pPN, Value *pV, Value *jte, Value *context);

    //void handleGlobalFuncPtr(Module &M, GlobalVariable *pGV, Value *pV);
    void handleGlobalVariables(Module &M);
    void handleGlobalVariable(Module &M, GlobalVariable *pGV);
    void handleFunctionTy(Module &M, Value *pV, GlobalVariable *pGV, vector<Value*> *indices, Function *pF);
    void handleArrayTy(Module &M, Constant *pConst, GlobalVariable *pGV, vector<Value*> *indices);
    void handleStructTy(Module &M, Constant *pConst, GlobalVariable *pGV, vector<Value*> *indices);
		void handleIndirectCalls(Module &M);
		void handleIndirectCall(Module &M, Instruction *pI);
		void init(Module &M);
    void printFuncAddr(Module &M);
    void insertAcfiSet(Module &M, int enable);
    Value *insertTagi(Module &M, IRBuilder<> *Builder, Value *pV, Value *pF);
    Value *insertTagd(Module &M, IRBuilder<> *Builder, Value *pV, Value *pF);
    Value *insertAuti(Module &M, IRBuilder<> *Builder, Value *pV, Value *pF);
    Value *insertXtag(Module &M, IRBuilder<> *Builder, Value *pV);
    void insertBsetm(Module &M, IRBuilder<> *Builder, Value *pV);
    Value *insertClosure(Module &M, IRBuilder<> *Builder, Value *pVa, Value *pVb, Value *context);
    //void insertEact(Module &M, IRBuilder<> *Builder, Value *pV, size_t label);
    //void insertEstr(Module &M, IRBuilder<> *Builder, Value *pV, size_t label);
    void replaceOp(Value *pVa, Value *pVb, Instruction *pI);
    void handleIntrinsicFunctions(Module &M);
    void buildTypeString(const Type *T, llvm::raw_string_ostream &O);
    uint64_t getTypeIDFor(const Type *T);
    Constant *getTypeIDConstantFrom(const Type &T, LLVMContext &C);

    u_int16_t max_label = 0;
    unsigned statNumIndirectCall = 0;
    unsigned statNumESTR = 0;
    unsigned statMaxEqClassSize = 0;
    unsigned getNextPow2(unsigned n) {
      n--;
      n |= n >> 1;
      n |= n >> 2;
      n |= n >> 4;
      n |= n >> 8;
      n |= n >> 16;
      n++;
      return n;
    }
  };
//}
