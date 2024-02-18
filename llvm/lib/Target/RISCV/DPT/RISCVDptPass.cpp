#include "RISCV.h"
#include "RISCVSubtarget.h"
#include "RISCVRegisterInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "RISCVMachineFunctionInfo.h"
#include "MCTargetDesc/RISCVInstPrinter.h"

#include "llvm/DPT/DPT.h"

#define DEBUG_TYPE "RISCVDptPass"

STATISTIC(StatNumTagd,
            DEBUG_TYPE "Number of tagd intrinsics replaced");

STATISTIC(StatNumXtag,
            DEBUG_TYPE "Number of xtag intrinsics replaced");

STATISTIC(StatNumCstr,
            DEBUG_TYPE "Number of capstr intrinsics replaced");

STATISTIC(StatNumCclr,
            DEBUG_TYPE "Number of capclr intrinsics replaced");

//STATISTIC(StatNumCsrch,
//            DEBUG_TYPE "Number of Csrch intrinsics replaced");

using namespace llvm;

namespace {
 class RISCVDptPass : public MachineFunctionPass {

 public:
   static char ID;

   RISCVDptPass() : MachineFunctionPass(ID) {}

   StringRef getPassName() const override { return DEBUG_TYPE; }

   virtual bool doInitialization(Module &M) override;
   bool runOnMachineFunction(MachineFunction &) override;

 private:
   const RISCVSubtarget *STI = nullptr;
   const RISCVInstrInfo *TII = nullptr;
   inline bool handleInstruction(MachineFunction &MF, MachineBasicBlock &MBB,
                                      MachineBasicBlock::instr_iterator &MIi);
  };
}

char RISCVDptPass::ID = 0;
FunctionPass *llvm::createRISCVDptPass() { return new RISCVDptPass(); }

bool RISCVDptPass::doInitialization(Module &M) {
  return true;
}

bool RISCVDptPass::runOnMachineFunction(MachineFunction &MF) {
  bool modified = false;
  STI = &MF.getSubtarget<RISCVSubtarget>();
  TII = STI->getInstrInfo();

  for (auto &MBB : MF) {
    for (auto MIi = MBB.instr_begin(), MIie = MBB.instr_end(); MIi != MIie;) {
			//(*MIi).dump();
      auto MIk = MIi++;

      switch (MIk->getOpcode()) {
        case RISCV::PA_TAGD: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::TAGD), MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg())
						//.addReg(RISCV::X0);
						//.addReg(RISCV::X2); // X2: stack pointer
            .addReg(MIk->getOperand(2).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumTagd;

          break;
        }
        case RISCV::PA_XTAG: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::XTAG), MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumXtag;

          break;
        }
        case RISCV::DPT_CSTR: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::CSTR))
            .addReg(MIk->getOperand(0).getReg())
						.addReg(MIk->getOperand(1).getReg());
	
          MIk->removeFromParent();
          modified = true;
          ++StatNumCstr;

          break;
        }
        case RISCV::DPT_CCLR: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::CCLR))
            .addReg(MIk->getOperand(0).getReg())
						.addReg(RISCV::X0); // X0: zero register
	
          MIk->removeFromParent();
          modified = true;
          ++StatNumCclr;

          break;
        }
        case RISCV::BM_BSETM: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::BSETM), RISCV::X0)
            .addReg(MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(0).getReg());

          MIk->removeFromParent();
          modified = true;
          //++StatNumBsetm;
          break;
        }
        case RISCV::BM_BCLRM: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::BCLRM), RISCV::X0)
            .addReg(MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(0).getReg());

          MIk->removeFromParent();
          modified = true;
          //++StatNumBclrm;
          break;
        }
        case RISCV::BM_BEXTM: {
          //errs() << "Found BEXTM!\n";
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::BEXTM), MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg())
            .addReg(MIk->getOperand(1).getReg());

          MIk->removeFromParent();
          modified = true;
          //++StatNumBextm;
          break;
        }
        default:
          break;
      }
    }
  }

  return modified;
}

