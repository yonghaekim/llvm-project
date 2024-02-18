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
  class IFCCPass : public ModulePass {

  public:
    static char ID; // Pass identification, replacement for typeid
    IFCCPass() : ModulePass(ID) {}
    bool runOnModule(Module &M) override;
    void handleBitCastInsts(Module &M);

  	set<Function*> white_set;
    map<unsigned, list<Function*>*> fl_map;
    map<unsigned, Function*> jt_map;
	  map<CallBase*,Value*> call_map;


  private:  
    bool LARGE_ALIGN = false;
		bool QEMU = false;
    char *voidptr = nullptr;
		LLVMContext *C = nullptr;
		const DataLayout *DL = nullptr;
    Function *main = nullptr;
    Function *init_acfi = nullptr;
    unsigned int temp_cnt = 0;
    unsigned int func_num = 0;
    unsigned int call_num = 0;

    void findWhiteSet(Module &M);
    void createJumpTables(Module &M);
    void replaceUsers(Module &M);
    void findDirectCalls(Module &M, Value *pV, set<CallBase*> &call_set);
    void createJumpTableEntries(Module &M);
    void handleIndirectCalls(Module &M);
    void handleIndirectCall(Module &M, CallBase *pCB);
    void printFuncAddr(Module &M);
    void printFuncNum(Module &M);
		void init(Module &M);
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

    unsigned statMaxEqClassSize = 0;
    unsigned statNumIndirectCall = 0;
  };
//}
