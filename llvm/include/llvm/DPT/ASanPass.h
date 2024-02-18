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
  class ASanPass : public ModulePass {

  public:
    static char ID; // Pass identification, replacement for typeid
    ASanPass() : ModulePass(ID) {}

//    map<Value *, DptNode *> value_map; 
//    //set<DptNode *> visit_set;
//		list<Instruction *> inst_list;
//
//		set<DptNode *> taint_nodes;
//		map<Type *, set<unsigned>> taint_indices;
//		map<Type *, set<unsigned>> signed_indices;
//		set<Value *> sign_set;
//		list<vector<Value *>> indices_list;
    set<GetElementPtrInst *> gep_visit_set;
    set<GetElementPtrInst *> gep_erase_set;

		LLVMContext *C = NULL;
		const DataLayout *DL = NULL;
		Function *main = nullptr;
		Function *entry = nullptr;
    char *voidptr = nullptr;

    bool runOnModule(Module &M) override;
		//void getAnalysisUsage(AnalysisUsage &AU) const;

    unsigned temp_cnt = 0;
    unsigned intr_num = 0;
    unsigned func_num = 0;
    unsigned tagd_cnt = 0;

  private:  
		void handleGlobalVariables(Module &M);
		void handleGlobalVariable(Module &M, GlobalVariable *pGV);
		void handleAllocas(Module &M);
		void handleAlloca(Module &M, AllocaInst *pAI);
		void handleMemInsts(Module &M);

		bool IsStructTy(Type *ty);
    bool IsStructTyWithArray(Type *ty);
    set<Type *> getStructTypes(Type *ty, set<Type *> type_set);
		void init(Module &M);
    void insertDptSet(Module &M);
    void printFuncAddr(Module &M);
  };
//}

