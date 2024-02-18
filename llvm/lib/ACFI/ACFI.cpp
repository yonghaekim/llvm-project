#include "llvm/ACFI/ACFI.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include <regex>
#include <map>

using namespace llvm;
using namespace ACFI;

static cl::opt<ACFI::AcfiType> AcfiInstType(
    "acfi-type", cl::init(AcfiType::None),
    cl::desc("Authenticated Control-Flow Integrity"),
    cl::value_desc("mode"),
    cl::values(clEnumValN(AcfiType::None, "base", "Disable protection"),
               clEnumValN(AcfiType::IFCC, "ifcc", "Enable IFCC"),
               clEnumValN(AcfiType::PA, "pa", "Enable PA"),
               clEnumValN(AcfiType::ACFI, "acfi", "Enable ACFI"),
               clEnumValN(AcfiType::ACFI_MC, "acfi-mc", "Enable ACFI+MC"),
               clEnumValN(AcfiType::ACFI_MCI, "acfi-mci", "Enable ACFI+MCI")));

AcfiType ACFI::getAcfiInstType() {
  return AcfiInstType;
}

static cl::opt<ACFI::AcfiQemuEn> AcfiQemuMode(
    "acfi-qemu", cl::init(AcfiQemuEn::Disable),
    cl::desc("ACFI QEMU mode control"),
    cl::value_desc("mode"),
    cl::values(clEnumValN(AcfiQemuEn::Disable, "disable", "Disable ACFI QEMU mode"),
               clEnumValN(AcfiQemuEn::Enable, "enable", "Enable ACFI QEMU mode")));

AcfiQemuEn ACFI::getAcfiQemuMode() {
  return AcfiQemuMode;
}
