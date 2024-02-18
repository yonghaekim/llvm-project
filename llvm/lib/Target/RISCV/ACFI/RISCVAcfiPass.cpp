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

#include "llvm/ACFI/ACFI.h"

#define DEBUG_TYPE "RISCVAcfiPass"

STATISTIC(StatNumTagc, DEBUG_TYPE "Number of tagi intrinsics replaced");
STATISTIC(StatNumAutc, DEBUG_TYPE "Number of auti intrinsics replaced");
STATISTIC(StatNumXtag, DEBUG_TYPE "Number of xtag intrinsics replaced");
STATISTIC(StatNumBsetm, DEBUG_TYPE "Number of bsetm intrinsics replaced");
STATISTIC(StatNumBclrm, DEBUG_TYPE "Number of bclrm intrinsics replaced");
STATISTIC(StatNumBextm, DEBUG_TYPE "Number of bextm intrinsics replaced");
STATISTIC(StatNumEstr, DEBUG_TYPE "Number of estr intrinsics replaced");
STATISTIC(StatNumEclr, DEBUG_TYPE "Number of eclr intrinsics replaced");
STATISTIC(StatNumEchk, DEBUG_TYPE "Number of echk intrinsics replaced");
STATISTIC(StatNumEact, DEBUG_TYPE "Number of eact intrinsics replaced");
STATISTIC(StatNumEdea, DEBUG_TYPE "Number of edea intrinsics replaced");

using namespace llvm;

namespace {
 class RISCVAcfiPass : public MachineFunctionPass {

 public:
   static char ID;

   RISCVAcfiPass() : MachineFunctionPass(ID) {}

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

char RISCVAcfiPass::ID = 0;
FunctionPass *llvm::createRISCVAcfiPass() { return new RISCVAcfiPass(); }

bool RISCVAcfiPass::doInitialization(Module &M) {
  return true;
}

bool RISCVAcfiPass::runOnMachineFunction(MachineFunction &MF) {
  bool modified = false;
  STI = &MF.getSubtarget<RISCVSubtarget>();
  TII = STI->getInstrInfo();

	//if (MF.getFunction().getName() == "__cfi_jumptable") {
	//  for (auto &MBB : MF) {
	//		for (auto MIi = MBB.instr_begin(), MIie = MBB.instr_end(); MIi != MIie;) {
	//			(*MIi).dump();
	//			auto MIk = MIi++;
	//		}
	//	}
	//}

  for (auto &MBB : MF) {
    for (auto MIi = MBB.instr_begin(), MIie = MBB.instr_end(); MIi != MIie;) {
			//(*MIi).dump();
      auto MIk = MIi++;

      switch (MIk->getOpcode()) {
        case RISCV::PA_TAGC: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::TAGC), MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg())
            .addReg(MIk->getOperand(2).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumTagc;
          break;
        }
        case RISCV::PA_AUTC: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::AUTC), MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg())
            .addReg(MIk->getOperand(2).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumAutc;
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
        case RISCV::ACFI_ESTR: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::ESTR))
            .addReg(MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumEstr;
          break;
        }
        case RISCV::ACFI_ECLR: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::ECLR))
            .addReg(MIk->getOperand(0).getReg())
            .addReg(RISCV::X0);

          MIk->removeFromParent();
          modified = true;
          ++StatNumEclr;
          break;
        }
        case RISCV::ACFI_ECHK: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::ECHK))
            .addReg(MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumEchk;
          break;
        }
        case RISCV::ACFI_EACT: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::EACT))
            .addReg(MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumEact;
          break;
        }
        case RISCV::ACFI_EDEA: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::EDEA))
            .addReg(MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumEdea;
          break;
        }
        case RISCV::BM_BSETM: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::BSETM), RISCV::X0)
            .addReg(MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(0).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumBsetm;
          break;
        }
        case RISCV::BM_BCLRM: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::BCLRM), RISCV::X0)
            .addReg(MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(0).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumBclrm;
          break;
        }
        case RISCV::BM_BEXTM: {
          BuildMI(MBB, MIk, MIk->getDebugLoc(), TII->get(RISCV::BEXTM), MIk->getOperand(0).getReg())
            .addReg(MIk->getOperand(1).getReg())
            .addReg(MIk->getOperand(1).getReg());

          MIk->removeFromParent();
          modified = true;
          ++StatNumBextm;
          break;
        }

        default:
          break;
      }
    }
  }

  return modified;
}

