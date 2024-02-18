#ifndef LLVM_ACFI_H
#define LLVM_ACFI_H

#include "llvm/IR/Constant.h"
#include "llvm/IR/Type.h"
#include "llvm/Pass.h"

namespace llvm {
namespace ACFI {

enum AcfiType {
  None,
  IFCC,
  PA,
  ACFI,
  ACFI_MC,
  ACFI_MCI
};

enum AcfiQemuEn {
	Disable,
	Enable
};

AcfiType getAcfiInstType();
AcfiQemuEn getAcfiQemuMode();
//Pass *createJumpTableGenPass();
//Pass *createFuncAddrSignPass();
//Pass *createShadowCallMemPass();
Pass *createIFCCPass();
Pass *createPAPass();
Pass *createACFIPass();
Pass *createMCPass();
}
} // llvm

#endif //LLVM_ACFI_H
