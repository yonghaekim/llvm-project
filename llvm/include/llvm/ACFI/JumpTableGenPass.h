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
  class JumpTableGenPass : public ModulePass {

  public:
    static char ID; // Pass identification, replacement for typeid
    JumpTableGenPass() : ModulePass(ID) {}
    bool runOnModule(Module &M) override;
    set<Function*> getWhiteSet();
    map<unsigned, list<Function*>*> getFuncListMap();
    map<unsigned, Function*> getJumpTableMap();
    void handleBitCastInsts(Module &M);

    set<string> black_set;
  	set<Function*> white_set;
    map<unsigned, list<Function*>*> fl_map;
    map<unsigned, Function*> jt_map;

  private:  
    bool LARGE_ALIGN = false;
    bool IFCC = false;
    bool PA = false;
    bool ACFI = false;
		bool QEMU = false;
    char *voidptr = nullptr;
		LLVMContext *C = nullptr;
		const DataLayout *DL = nullptr;

    void initBlackSet();
    void findWhiteSet(Module &M);
    void makeJumpTables(Module &M);
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
  };
//}
