#include "llvm/DPT/DPT.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include <regex>
#include <map>

using namespace llvm;
using namespace DPT;

//static cl::opt<bool> EnableFuncCounter("wyfy-count", cl::Hidden,
//                                      cl::desc("DPT function counter pass"),
//                                      cl::init(false));

static cl::opt<DPT::DptType> DptInstType(
    "dpt-type", cl::init(DptType::None),
    cl::desc("DPT inst type"),
    cl::value_desc("inst"),
    cl::values(clEnumValN(DptType::None, "base", "Disable protection"),
               clEnumValN(DptType::ASAN, "asan", "Enable custom address sanitizer"),
               clEnumValN(DptType::DPT_H, "dpt-h", "Enable heap-only protection"),
               clEnumValN(DptType::DPT_C, "dpt-c", "Enable coarse-grained protection"),
               clEnumValN(DptType::DPT_F, "dpt-f", "Enable fine-grained protection")));

DptType DPT::getDptInstType() {
  return DptInstType;
}

static cl::opt<DPT::DptQemu> DptQemuMode(
    "dpt-qemu", cl::init(DptQemu::Disable),
    cl::desc("DPT QEMU mode control"),
    cl::value_desc("qemu"),
    cl::values(clEnumValN(DptQemu::Disable, "disable", "Disable DPT QEMU mode"),
               clEnumValN(DptQemu::Enable, "enable", "Enable DPT QEMU mode")));

DptQemu DPT::getDptQemuMode() {
  return DptQemuMode;
}

static cl::opt<DptTest> DptTestMode(
    "dpt-test", cl::init(DptTest::Mode0),
    cl::desc("DPT test mode"),
    cl::value_desc("test"),
    cl::values(clEnumValN(DptTest::Mode0, "mode0", "DPT w/o SHB w/ 1 CMT"),
               clEnumValN(DptTest::Mode1, "mode1", "DPT w/  SHB w/ 32 CMT"),
               clEnumValN(DptTest::Mode2, "mode2", "AOS")));

DptTest DPT::getDptTestMode() {
  return DptTestMode;
}

static cl::opt<DptSpec> DptSpecMode(
    "dpt-spec", cl::init(DptSpec::SpecEnable),
    cl::desc("DPT spec mode"),
    cl::value_desc("spec"),
    cl::values(clEnumValN(DptSpec::SpecEnable, "enable", "Enable speculative capability checks"),
               clEnumValN(DptSpec::SpecDisable, "disable", "Disable speculative capability checks")));

DptSpec DPT::getDptSpecMode() {
  return DptSpecMode;
}

static cl::opt<DptFault> DptFaultMode(
    "dpt-fault", cl::init(DptFault::FaultEnable),
    cl::desc("DPT fault mode"),
    cl::value_desc("fault"),
    cl::values(clEnumValN(DptFault::FaultEnable, "enable", "Enable fault detection"),
               clEnumValN(DptFault::FaultDisable, "disable", "Disable fault detection")));

DptFault DPT::getDptFaultMode() {
  return DptFaultMode;
}

static cl::opt<DptXtag> DptXtagMode(
    "dpt-xtag", cl::init(DptXtag::XtagDisable),
    cl::desc("DPT xtag mode"),
    cl::value_desc("xtag"),
    cl::values(clEnumValN(DptXtag::XtagEnable, "enable", "Insert xtags"),
               clEnumValN(DptXtag::XtagDisable, "disable", "Don't insert xtags")));

DptXtag DPT::getDptXtagMode() {
  return DptXtagMode;
}

static cl::opt<DptMaxWays> DptMaxWaysNum(
    "dpt-max-ways", cl::init(DptMaxWays::MaxWays8),
    cl::desc("DPT max ways"),
    cl::value_desc("max-ways"),
    cl::values(clEnumValN(DptMaxWays::MaxWays8, "8", "Max ways: 8"),
               clEnumValN(DptMaxWays::MaxWays16, "16", "Max ways: 16"),
               clEnumValN(DptMaxWays::MaxWays32, "32", "Max ways: 32"),
               clEnumValN(DptMaxWays::MaxWays64, "64", "Max ways: 64"),
               clEnumValN(DptMaxWays::MaxWays128, "128", "Max ways: 128"),
               clEnumValN(DptMaxWays::MaxWays256, "256", "Max ways: 256")));

DptMaxWays DPT::getDptMaxWaysNum() {
  return DptMaxWaysNum;
}
