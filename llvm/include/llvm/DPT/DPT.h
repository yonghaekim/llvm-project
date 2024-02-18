#ifndef LLVM_DPT_H
#define LLVM_DPT_H

#include "llvm/IR/Constant.h"
#include "llvm/IR/Type.h"
#include "llvm/Pass.h"

namespace llvm {
namespace DPT {

enum DptType {
  None,
  ASAN,
  DPT_H,
  DPT_C,
  DPT_F
};

enum DptQemu {
	Disable,
	Enable
};

enum DptTest {
	Mode0,
	Mode1,
	Mode2
};

enum DptSpec {
  SpecEnable,
  SpecDisable
};

enum DptFault {
  FaultEnable,
  FaultDisable
};

enum DptXtag {
  XtagEnable,
  XtagDisable
};

enum DptMaxWays {
  MaxWays8,
  MaxWays16,
  MaxWays32,
  MaxWays64,
  MaxWays128,
  MaxWays256
};

DptType getDptInstType();
DptQemu getDptQemuMode();
DptTest getDptTestMode();
DptSpec getDptSpecMode();
DptFault getDptFaultMode();
DptXtag getDptXtagMode();
DptMaxWays getDptMaxWaysNum();
Pass *createDataPtrTagPass();
Pass *createASanPass();
}
} // llvm

#endif //LLVM_DPT_H
