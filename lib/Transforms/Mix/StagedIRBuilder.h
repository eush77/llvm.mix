//===- StagedIRBuilder.h ----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the helper class used to build LLVM IR at a later
// execution stage.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H
#define LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H

#include "CAPIFunctions.h"
#include "MixContext.h"
#include "Types.h"

#include "llvm-c/Core.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/IR/Argument.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"

#include <cassert>
#include <functional>
#include <iterator>
#include <memory>
#include <string>
#include <tuple>
#include <unordered_map>
#include <utility>

using namespace std::string_literals;

namespace llvm {

namespace mix {

// This class provides a wrapper around IRBuilder to build code that builds
// other code when executed, using LLVM library API.
template <typename IRBuilder> class StagedIRBuilder {
public:
  StagedIRBuilder(IRBuilder &Builder, const MixContext &C) : B(Builder), C(C) {}
  StagedIRBuilder(const StagedIRBuilder &Other) = delete;
  StagedIRBuilder(StagedIRBuilder &&Other) = default;

  IRBuilder &getBuilder() const { return B; }

  // Interface to current staged function/block.
  Instruction *getFunction() const { return SF; }
  Instruction *getBasicBlock() const { return SBB; }

  // Interface to particular LLVM API calls.
  Instruction *createFunction(FunctionType *Type,
                              GlobalValue::LinkageTypes Linkage,
                              const Twine &Name = "",
                              const Twine &InstName = "",
                              bool SetFunction = false);
  Instruction *setLinkage(Value *Global, GlobalValue::LinkageTypes Linkage);
  Instruction *setName(Instruction *I, StringRef Name);
  Instruction *positionBuilderAtEnd(Instruction *StagedBasicBlock,
                                    const Twine &InstName = "");

  // Stage a value by inserting commands to build it, unless it exists, in
  // which case return the previous value. Staging a value that is a basic
  // block means creating an empty basic block in the staged function.
  Instruction *stage(Value *V);
  Instruction *stage(Argument *A, unsigned ArgNo);
  Instruction *stage(BasicBlock *BB, bool SetBasicBlock);
  Instruction *stage(std::unique_ptr<Instruction, ValueDeleter> &&I) {
    return stageInstruction(I.get());
  }

  void setStagedValue(Value *V, Instruction *StagedV);

  // Define static value in the generated function if it is defined elsewhere.
  Value *defineStatic(Value *V);
  Value *defineStatic(Argument *A, Value *SA);
  BasicBlock *defineStatic(BasicBlock *BB);
  Instruction *defineStatic(Instruction *I, Instruction *V = nullptr);
  PHINode *defineStatic(PHINode *Phi, bool Staged);

  // Stage a static value as a constant in generated code.
  Instruction *stageStatic(Value *V);

  // Stage a string by creating a global string variable with a given
  // initializer and returning an `i8*' pointer to the first character.
  Value *stage(StringRef Name, const Twine &VarName = "mix.name") {
    return B.CreateGlobalStringPtr(Name, VarName);
  }

  // Change target of a dynamic call.
  void setCalledValue(Instruction *Call, Instruction *V);

private:
  Module &getModule() const { return *B.GetInsertBlock()->getModule(); }
  Instruction *stageBasicBlock(BasicBlock *Block);
  void stageIncomingList(PHINode *Phi, Instruction *StagedPhi);
  Instruction *stageInstruction(Instruction *Inst);
  void stageInstructionMetadata(Instruction *StagedInst, Instruction *Inst);
  Instruction *stageMetadata(Metadata *);
  Instruction *stageMDNode(MDNode *);

  // Register a callback to be called when the value is staged. If the value
  // has already been staged, call it immediately.
  void whenStaged(Value *V, std::function<void(Instruction *)> Callback);

  IRBuilder &B;
  MixContext C;
  Instruction *SF = nullptr;
  Instruction *SBB = nullptr;
  DenseMap<Value *, Instruction *> StagedValues;
  std::unordered_multimap<Value *, std::function<void(Instruction *)>>
      StageCallbacks;
  DenseMap<Argument *, Value *> StaticArguments;
  DenseMap<BasicBlock *, BasicBlock *> StaticBasicBlocks;
  DenseMap<Instruction *, Instruction *> StaticInstructions;
};

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::createFunction(
    FunctionType *Type, GlobalValue::LinkageTypes Linkage, const Twine &Name,
    const Twine &InstName, bool SetFunction) {
  auto *F = B.CreateCall(getAddFunctionFn(getModule()),
                         {C.getModule(), stage(Name.str()), C.getType(Type)},
                         InstName);

  if (Linkage != GlobalValue::ExternalLinkage) {
    setLinkage(F, Linkage);
  }

  if (SetFunction)
    SF = F;

  return F;
}

template <typename IRBuilder>
Instruction *
StagedIRBuilder<IRBuilder>::setLinkage(Value *Global,
                                       GlobalValue::LinkageTypes Linkage) {
  LLVMLinkage CAPILinkage;

  switch (Linkage) {
  case GlobalValue::ExternalLinkage:
    CAPILinkage = LLVMExternalLinkage;
    break;

  case GlobalValue::AvailableExternallyLinkage:
    CAPILinkage = LLVMAvailableExternallyLinkage;
    break;

  case GlobalValue::LinkOnceAnyLinkage:
    CAPILinkage = LLVMLinkOnceAnyLinkage;
    break;

  case GlobalValue::LinkOnceODRLinkage:
    CAPILinkage = LLVMLinkOnceODRLinkage;
    break;

  case GlobalValue::WeakAnyLinkage:
    CAPILinkage = LLVMWeakAnyLinkage;
    break;

  case GlobalValue::WeakODRLinkage:
    CAPILinkage = LLVMWeakODRLinkage;
    break;

  case GlobalValue::AppendingLinkage:
    CAPILinkage = LLVMAppendingLinkage;
    break;

  case GlobalValue::InternalLinkage:
    CAPILinkage = LLVMInternalLinkage;
    break;

  case GlobalValue::PrivateLinkage:
    CAPILinkage = LLVMPrivateLinkage;
    break;

  case GlobalValue::ExternalWeakLinkage:
    CAPILinkage = LLVMExternalWeakLinkage;
    break;

  case GlobalValue::CommonLinkage:
    CAPILinkage = LLVMCommonLinkage;
    break;
  }

  return B.CreateCall(
      getSetLinkageFn(getModule()),
      {Global, ConstantInt::get(getLinkageTy(B.getContext()), CAPILinkage)});
}

// Set name of a staged value.
template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::setName(Instruction *I,
                                                 StringRef Name) {
  return B.CreateCall(getSetValueNameFn(getModule()), {I, stage(Name)});
}

template <typename IRBuilder>
Instruction *
StagedIRBuilder<IRBuilder>::positionBuilderAtEnd(Instruction *StagedBlock,
                                                 const Twine &InstName) {
  return B.CreateCall(getPositionBuilderAtEndFn(getModule()),
                      {C.getBuilder(), StagedBlock}, InstName);
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(Argument *Arg, unsigned ArgNo) {
  assert(!StagedValues.count(Arg) && "Argument has already been staged");

  Instruction *StagedArg = B.CreateCall(
      getGetParamFn(getModule()),
      {SF, ConstantInt::get(getUnsignedIntTy(B.getContext()), ArgNo)},
      Arg->getName());

  setStagedValue(Arg, StagedArg);
  return StagedArg;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(BasicBlock *BB,
                                               bool SetBasicBlock) {
  Instruction *SBB = stage(BB);

  if (SetBasicBlock)
    this->SBB = SBB;

  return SBB;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageBasicBlock(BasicBlock *Block) {
  return B.CreateCall(getAppendBasicBlockInContextFn(getModule()),
                      {C.getContext(), SF, stage(Block->getName())},
                      Block->getName());
}

// Stage list of incoming values and add them to the already staged Phi node.
// This can't always happen right away because incoming values may not be
// ready yet, so add hooks to build the list when they will be.
template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::stageIncomingList(PHINode *Phi,
                                                   Instruction *StagedPhi) {
  auto *IncomingValueAlloca =
      B.CreateAlloca(getValuePtrTy(B.getContext()), B.getInt32(1));
  auto *IncomingBlockAlloca =
      B.CreateAlloca(getBasicBlockPtrTy(B.getContext()), B.getInt32(1));

  for (unsigned IncomingNum = 0; IncomingNum < Phi->getNumIncomingValues();
       ++IncomingNum) {
    whenStaged(Phi->getIncomingValue(IncomingNum),
               [=](Instruction *StagedIncomingValue) {
                 B.CreateStore(StagedIncomingValue, IncomingValueAlloca);
                 B.CreateStore(stage(Phi->getIncomingBlock(IncomingNum)),
                               IncomingBlockAlloca);

                 B.CreateCall(
                     getAddIncomingFn(getModule()),
                     {StagedPhi, IncomingValueAlloca, IncomingBlockAlloca,
                      ConstantInt::get(getUnsignedIntTy(B.getContext()), 1)});
               });
  }
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageInstruction(Instruction *Inst) {
  SmallVector<Type *, 4> ParamTypes{getBuilderPtrTy(B.getContext())};
  SmallVector<Value *, 4> Arguments{C.getBuilder()};
  const char *BuilderName = nullptr;

  auto pushArg = [&ParamTypes, &Arguments](Type *Ty, Value *V) {
    assert(V->getType() == Ty && "Argument type mismatch");

    ParamTypes.push_back(Ty);
    Arguments.push_back(V);
  };

  auto setBuilderName = [&BuilderName](const char *Name) {
    assert(!BuilderName && "Builder name is already set");
    BuilderName = Name;
  };

  if (Inst->isBinaryOp()) {
    LLVMOpcode CAPIOpcode = LLVMGetInstructionOpcode(wrap(Inst));

    pushArg(getOpcodeTy(B.getContext()),
            ConstantInt::get(getOpcodeTy(B.getContext()), CAPIOpcode));
    pushArg(getValuePtrTy(B.getContext()), stage(Inst->getOperand(0)));
    pushArg(getValuePtrTy(B.getContext()), stage(Inst->getOperand(1)));
    pushArg(getCharPtrTy(B.getContext()), stage(Inst->getName()));
    setBuilderName("BinOp");
  } else {
    switch (Inst->getOpcode()) {
    case Instruction::Alloca: {
      auto *Alloca = cast<AllocaInst>(Inst);

      pushArg(getTypePtrTy(B.getContext()),
              C.getType(Alloca->getAllocatedType()));
      if (Alloca->isArrayAllocation()) {
        pushArg(getValuePtrTy(B.getContext()), stage(Alloca->getArraySize()));
        setBuilderName("ArrayAlloca");
      } else {
        setBuilderName("Alloca");
      }
      pushArg(getCharPtrTy(B.getContext()), stage(Inst->getName()));
      break;
    }

    case Instruction::Br: {
      auto *Br = cast<BranchInst>(Inst);

      if (Br->isConditional()) {
        pushArg(getValuePtrTy(B.getContext()), stage(Br->getCondition()));
        pushArg(getBasicBlockPtrTy(B.getContext()), stage(Br->getSuccessor(0)));
        pushArg(getBasicBlockPtrTy(B.getContext()), stage(Br->getSuccessor(1)));
        setBuilderName("CondBr");
      } else {
        pushArg(getBasicBlockPtrTy(B.getContext()), stage(Br->getSuccessor(0)));
        setBuilderName("Br");
      }
      break;
    }

    case Instruction::Call: {
      auto *Call = cast<CallInst>(Inst);

      Value *Args = B.CreateAlloca(getValuePtrTy(B.getContext()),
                                   B.getInt32(Call->getNumArgOperands()));

      for (unsigned ArgNum = 0; ArgNum < Call->getNumArgOperands(); ++ArgNum) {
        B.CreateStore(stage(Call->getArgOperand(ArgNum)),
                      B.CreateGEP(Args, B.getInt32(ArgNum)));
      }

      pushArg(getValuePtrTy(B.getContext()), stage(Call->getCalledValue()));
      pushArg(getValuePtrTy(B.getContext())->getPointerTo(), Args);
      pushArg(getUnsignedIntTy(B.getContext()),
              ConstantInt::get(getUnsignedIntTy(B.getContext()),
                               Call->getNumArgOperands()));
      pushArg(getCharPtrTy(B.getContext()), stage(Call->getName()));
      setBuilderName("Call");
      break;
    }

    case Instruction::ExtractValue: {
      auto *EV = cast<ExtractValueInst>(Inst);
      assert(EV->getNumIndices() == 1 &&
             "Unsupported number of indices for extractvalue");

      pushArg(getValuePtrTy(B.getContext()), stage(EV->getAggregateOperand()));
      pushArg(getUnsignedIntTy(B.getContext()),
              ConstantInt::get(getUnsignedIntTy(B.getContext()),
                               EV->getIndices().front()));
      pushArg(getCharPtrTy(B.getContext()), stage(Inst->getName()));
      setBuilderName("ExtractValue");
      break;
    }

    case Instruction::GetElementPtr: {
      auto *GEP = cast<GetElementPtrInst>(Inst);

      pushArg(getValuePtrTy(B.getContext()), stage(GEP->getPointerOperand()));

      if (GEP->isInBounds() && GEP->getNumIndices() == 2 &&
          GEP->hasAllConstantIndices() &&
          cast<ConstantInt>(GEP->idx_begin())->isZero()) {
        pushArg(getUnsignedIntTy(B.getContext()), *std::next(GEP->idx_begin()));
        pushArg(getCharPtrTy(B.getContext()), stage(GEP->getName()));
        setBuilderName("StructGEP");
      } else {
        Value *Indices = B.CreateAlloca(getValuePtrTy(B.getContext()),
                                        B.getInt32(GEP->getNumIndices()));
        auto IIter = GEP->idx_begin();

        for (unsigned INum = 0; INum < GEP->getNumIndices(); ++INum)
          B.CreateStore(stage(*IIter++),
                        B.CreateGEP(Indices, B.getInt32(INum)));

        pushArg(getValuePtrTy(B.getContext())->getPointerTo(), Indices);
        pushArg(getUnsignedIntTy(B.getContext()),
                ConstantInt::get(getUnsignedIntTy(B.getContext()),
                                 GEP->getNumIndices()));
        pushArg(getCharPtrTy(B.getContext()), stage(GEP->getName()));
        setBuilderName(GEP->isInBounds() ? "InBoundsGEP" : "GEP");
      }
      break;
    }

    case Instruction::ICmp: {
      LLVMIntPredicate CAPIIntPredicate = LLVMGetICmpPredicate(wrap(Inst));

      pushArg(getIntPredicateTy(B.getContext()),
              ConstantInt::get(getIntPredicateTy(B.getContext()),
                               CAPIIntPredicate));
      pushArg(getValuePtrTy(B.getContext()), stage(Inst->getOperand(0)));
      pushArg(getValuePtrTy(B.getContext()), stage(Inst->getOperand(1)));
      pushArg(getCharPtrTy(B.getContext()), stage(Inst->getName()));
      setBuilderName("ICmp");
      break;
    }

    case Instruction::InsertValue: {
      auto *IV = cast<InsertValueInst>(Inst);
      assert(IV->getNumIndices() == 1 &&
             "Unsupported number of indices for insertvalue");

      pushArg(getValuePtrTy(B.getContext()), stage(IV->getAggregateOperand()));
      pushArg(getValuePtrTy(B.getContext()),
              stage(IV->getInsertedValueOperand()));
      pushArg(getUnsignedIntTy(B.getContext()),
              ConstantInt::get(getUnsignedIntTy(B.getContext()),
                               IV->getIndices().front()));
      pushArg(getCharPtrTy(B.getContext()), stage(Inst->getName()));
      setBuilderName("InsertValue");
      break;
    }

    case Instruction::Load: {
      auto *Load = cast<LoadInst>(Inst);

      pushArg(getValuePtrTy(B.getContext()), stage(Load->getPointerOperand()));
      pushArg(getCharPtrTy(B.getContext()), stage(Inst->getName()));
      setBuilderName("Load");
      break;
    }

    case Instruction::PHI: {
      auto *Phi = cast<PHINode>(Inst);

      pushArg(getTypePtrTy(B.getContext()), C.getType(Phi->getType()));
      pushArg(getCharPtrTy(B.getContext()), stage(Inst->getName()));
      setBuilderName("Phi");
      break;
    }

    case Instruction::Store: {
      auto *Store = cast<StoreInst>(Inst);

      pushArg(getValuePtrTy(B.getContext()), stage(Store->getValueOperand()));
      pushArg(getValuePtrTy(B.getContext()), stage(Store->getPointerOperand()));
      setBuilderName("Store");
      break;
    }

    case Instruction::Ret: {
      auto *Ret = cast<ReturnInst>(Inst);

      if (auto *ReturnValue = Ret->getReturnValue()) {
        pushArg(getValuePtrTy(B.getContext()), stage(ReturnValue));
        setBuilderName("Ret");
      } else {
        setBuilderName("RetVoid");
      }
      break;
    }

    case Instruction::Trunc: {
      auto *Trunc = cast<TruncInst>(Inst);

      pushArg(getValuePtrTy(B.getContext()), stage(Trunc->getOperand(0)));
      pushArg(getTypePtrTy(B.getContext()), C.getType(Trunc->getDestTy()));
      pushArg(getCharPtrTy(B.getContext()), stage(Inst->getName()));
      setBuilderName("Trunc");
      break;
    }

    case Instruction::Unreachable: {
      setBuilderName("Unreachable");
      break;
    }

    default:
      llvm_unreachable("Unsupported instruction");
    }
  }

  assert(BuilderName && "Builder name is not set");

  Instruction *StagedInst = B.CreateCall(
      getModule().getOrInsertFunction(
          "LLVMBuild"s + BuilderName,
          FunctionType::get(getValuePtrTy(B.getContext()), ParamTypes, false)),
      Arguments, Inst->getName());
  stageInstructionMetadata(StagedInst, Inst);
  return StagedInst;
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::stageInstructionMetadata(
    Instruction *StagedInst, Instruction *Inst) {
  SmallVector<std::pair<unsigned, MDNode *>, 2> MDs;
  Inst->getAllMetadata(MDs);

  for (auto &P : MDs) {
    unsigned KindID;
    MDNode *MDN;
    std::tie(KindID, MDN) = P;

    B.CreateCall(getSetMetadataFn(getModule()),
                 {StagedInst, C.getMDKindID(KindID), stageMetadata(MDN)});
  }
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageMetadata(Metadata *MD) {
  switch (MD->getMetadataID()) {
  case Metadata::ConstantAsMetadataKind:
  case Metadata::LocalAsMetadataKind: {
    auto *V = cast<ValueAsMetadata>(MD)->getValue();
    Value *MD = B.CreateCall(getValueAsMetadataFn(getModule()), {stage(V)});

    return B.CreateCall(getMetadataAsValueFn(getModule()),
                        {C.getContext(), MD});
  }

  case Metadata::MDStringKind: {
    auto *MDS = cast<MDString>(MD);

    return B.CreateCall(
        getMDStringInContextFn(getModule()),
        {C.getContext(), stage(MDS->getString()),
         ConstantInt::get(getUnsignedIntTy(B.getContext()), MDS->getLength())});
  }

  case Metadata::MDTupleKind:
    return stageMDNode(cast<MDNode>(MD));
  }

  llvm_unreachable("Unhandled metadata kind");
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageMDNode(MDNode *MDN) {
  Value *Vals = B.CreateAlloca(getValuePtrTy(B.getContext()),
                               B.getInt32(MDN->getNumOperands()));

  for (unsigned OpNum = 0; OpNum < MDN->getNumOperands(); ++OpNum) {
    B.CreateStore(stage(MetadataAsValue::get(B.getContext(),
                                             MDN->getOperand(OpNum).get())),
                  B.CreateGEP(Vals, B.getInt32(OpNum)));
  }

  return B.CreateCall(getMDNodeInContextFn(getModule()),
                      {C.getContext(), Vals,
                       ConstantInt::get(getUnsignedIntTy(B.getContext()),
                                        MDN->getNumOperands())});
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stage(Value *V) {
  if (isa<Constant>(V))
    return stageStatic(V);

  Instruction *StagedV = StagedValues.lookup(V);

  if (StagedV)
    return StagedV;

  if (auto *Block = dyn_cast<BasicBlock>(V)) {
    StagedV = stageBasicBlock(Block);
  } else if (auto *Inst = dyn_cast<Instruction>(V)) {
    StagedV = stageInstruction(Inst);

    if (auto *Phi = dyn_cast<PHINode>(Inst)) {
      stageIncomingList(Phi, StagedV);
    }
  } else if (auto *MDV = dyn_cast<MetadataAsValue>(V)) {
    StagedV = stageMetadata(MDV->getMetadata());
  }

  assert(StagedV && "Unsupported value kind");
  setStagedValue(V, StagedV);
  return StagedV;
}

template <typename IRBuilder>
Value *StagedIRBuilder<IRBuilder>::defineStatic(Argument *A, Value *SA) {
  if (A->getParent() == B.GetInsertBlock()->getParent())
    return A;

  if (SA) {
    assert(!StaticArguments.lookup(A) && "Argument has already been defined");
  } else {
    SA = StaticArguments.lookup(A);
    assert(SA && "Argument has not been defined");
    return SA;
  }

  return StaticArguments[A] = SA;
}

template <typename IRBuilder>
BasicBlock *StagedIRBuilder<IRBuilder>::defineStatic(BasicBlock *BB) {
  if (BB->getParent() == B.GetInsertBlock()->getParent())
    return BB;

  auto *SBB = StaticBasicBlocks.lookup(BB);

  if (SBB)
    return SBB;

  SBB = BasicBlock::Create(B.getContext(), BB->getName(),
                           B.GetInsertBlock()->getParent());
  return StaticBasicBlocks[BB] = SBB;
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::defineStatic(Instruction *I,
                                                      Instruction *V) {
  assert((!V || I->getType() == V->getType()) && "Type mismatch");

  if (I->getFunction() == B.GetInsertBlock()->getParent()) {
    assert(!V && "Cannot define this instruction");
    return I;
  }

  auto *SI = StaticInstructions.lookup(I);

  if (SI) {
    assert(!V && "Instruction is already defined");
    return SI;
  }

  if (V) {
    return StaticInstructions[I] = V;
  }

  assert(!isa<PHINode>(I) && "Static PHI nodes require an explicit type");

  SI = B.Insert(I->clone(), I->getName());

  // Remap operands.
  for (Use &Op : SI->operands()) {
    Op = defineStatic(Op);
  }

  return StaticInstructions[I] = SI;
}

// Static phi has either current or staged type depending on the binding type
// of its operands.
template <typename IRBuilder>
PHINode *StagedIRBuilder<IRBuilder>::defineStatic(PHINode *Phi, bool Staged) {
  if (Phi->getFunction() == B.GetInsertBlock()->getParent())
    return Phi;

  auto *SPhi = cast_or_null<PHINode>(StaticInstructions.lookup(Phi));

  if (SPhi)
    return SPhi;

  Type *Ty = Staged ? getValuePtrTy(B.getContext()) : Phi->getType();
  SPhi = B.CreatePHI(Ty, Phi->getNumIncomingValues(), Phi->getName());
  SPhi->moveBefore(B.GetInsertBlock()->getFirstNonPHI());

  for (unsigned IncomingNum = 0; IncomingNum < Phi->getNumIncomingValues();
       ++IncomingNum) {
    Value *IncomingV = Phi->getIncomingValue(IncomingNum);
    BasicBlock *IncomingBB = Phi->getIncomingBlock(IncomingNum);

    if (auto *IncomingI = dyn_cast<Instruction>(IncomingV)) {
      whenStaged(IncomingI, [=](Instruction *StagedI) {
        Value *IncomingV =
            Staged ? StagedI : this->StaticInstructions.lookup(IncomingI);

        SPhi->addIncoming(IncomingV, defineStatic(IncomingBB));
      });
    } else {
      IncomingBB = defineStatic(IncomingBB);

      // Staged value must be live on the edge from the incoming block.
      if (Staged) {
        auto IP = B.saveIP();

        if (auto *Term = IncomingBB->getTerminator()) {
          B.SetInsertPoint(Term);
        } else {
          B.SetInsertPoint(IncomingBB);
        }

        IncomingV = stage(IncomingV);

        B.restoreIP(IP);
      } else {
        IncomingV = defineStatic(IncomingV);
      }

      SPhi->addIncoming(IncomingV, IncomingBB);
    }
  }

  StaticInstructions[Phi] = SPhi;
  return SPhi;
}

template <typename IRBuilder>
Value *StagedIRBuilder<IRBuilder>::defineStatic(Value *V) {
  if (isa<Constant>(V))
    return V;

  if (auto *A = dyn_cast<Argument>(V)) {
    return defineStatic(A, nullptr);
  } else if (auto *BB = dyn_cast<BasicBlock>(V)) {
    return defineStatic(BB);
  } else if (auto *I = dyn_cast<Instruction>(V)) {
    return defineStatic(I);
  }

  llvm_unreachable("Unsupported value kind");
}

template <typename IRBuilder>
Instruction *StagedIRBuilder<IRBuilder>::stageStatic(Value *V) {
  Instruction *StagedV = StagedValues.lookup(V);

  if (StagedV)
    return StagedV;

  Value *StaticV = defineStatic(V);

  if (auto *StaticPhi = dyn_cast<PHINode>(StaticV)) {
    setStagedValue(V, StaticPhi);
    return StaticPhi;
  }

  if (isa<UndefValue>(StaticV)) {
    StagedV = B.CreateCall(getGetUndefFn(getModule()),
                           {C.getType(StaticV->getType())}, StaticV->getName());
    setStagedValue(V, StagedV);
    return StagedV;
  }

  switch (StaticV->getType()->getTypeID()) {
  case Type::VoidTyID: {
    StagedV = nullptr;          // No staged value
    break;
  }

  case Type::HalfTyID:
  case Type::FloatTyID:
  case Type::DoubleTyID:
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID: {
    Type *Ty = StaticV->getType();
    unsigned BitWidth = Ty->getPrimitiveSizeInBits();

    if (BitWidth <= getDoubleTy(B.getContext())->getPrimitiveSizeInBits()) {
      Value *Double =
          BitWidth < getDoubleTy(B.getContext())->getPrimitiveSizeInBits()
              ? B.CreateFPExt(StaticV, getDoubleTy(B.getContext()))
              : StaticV;
      StagedV = B.CreateCall(getConstRealFn(getModule()),
                             {C.getType(Ty), Double}, StaticV->getName());
    } else {
      Value *Int = stageStatic(B.CreateBitCast(StaticV, B.getIntNTy(BitWidth)));
      StagedV = B.CreateCall(getConstBitCastFn(getModule()),
                             {Int, C.getType(Ty)}, StaticV->getName());
    }
    break;
  }

  case Type::IntegerTyID: {
    IntegerType *Ty = cast<IntegerType>(StaticV->getType());
    unsigned BitWidth = Ty->getBitWidth();

    if (BitWidth <= getUnsignedLongLongIntTy(B.getContext())->getBitWidth()) {
      Value *ULL =
          BitWidth < getUnsignedLongLongIntTy(B.getContext())->getBitWidth()
              ? B.CreateZExt(StaticV, getUnsignedLongLongIntTy(B.getContext()))
              : StaticV;
      StagedV =
          B.CreateCall(getConstIntFn(getModule()),
                       {C.getType(Ty), ULL,
                        ConstantInt::get(getBoolTy(B.getContext()), false)},
                       StaticV->getName());
    } else {
      unsigned NumWords = alignTo(BitWidth, 64) / 64;
      Value *Words = B.CreateAlloca(B.getInt64Ty(), B.getInt32(NumWords));
      Value *Val = StaticV;

      for (unsigned WN = 0; WN < NumWords; ++WN) {
        B.CreateStore(B.CreateTrunc(Val, B.getInt64Ty()),
                      B.CreateGEP(Words, B.getInt32(WN)));
        Val = B.CreateLShr(Val, ConstantInt::get(Ty, 64));
      }

      StagedV = B.CreateCall(
          getConstIntOfArbitraryPrecisionFn(getModule()),
          {C.getType(Ty),
           ConstantInt::get(getUnsignedIntTy(B.getContext()), NumWords), Words},
          StaticV->getName());
    }
    break;
  }

  case Type::PointerTyID: {
    PointerType *Ty = cast<PointerType>(StaticV->getType());

    // Declare external functions instead of taking their address.
    // This is required for intrinsic calls to work, and otherwise makes for
    // more comprehensible code.
    if (Ty->getElementType()->isFunctionTy()) {
      if (auto *F = dyn_cast<Function>(StaticV->stripPointerCasts())) {
        if (F->hasExternalLinkage()) {
          StagedV = C.getFunction(F);

          if (StaticV->getType() != F->getType())
            StagedV = B.CreateCall(getBuildPointerCastFn(getModule()),
                                   {C.getBuilder(), StagedV,
                                    C.getType(StaticV->getType()),
                                    stage(StaticV->getName())});
          break;
        }
      }
    }

    Value *IntPtr =
        B.CreatePtrToInt(StaticV, getUnsignedLongLongIntTy(B.getContext()));

    StagedV = B.CreateCall(
        getConstIntToPtrFn(getModule()),
        {B.CreateCall(getConstIntFn(getModule()),
                      {C.getType(IntPtr->getType()), IntPtr,
                       ConstantInt::get(getBoolTy(B.getContext()), false)}),
         C.getType(Ty)},
        StaticV->getName());
    break;
  }

  case Type::StructTyID: {
    StructType *ST = cast<StructType>(StaticV->getType());
    Value *Elements;

    if (ST->getNumElements()) {
      Elements = B.CreateAlloca(getValuePtrTy(B.getContext()),
                                B.getInt32(ST->getNumElements()));

      for (unsigned ElNum = 0; ElNum < ST->getNumElements(); ++ElNum) {
        B.CreateStore(stageStatic(B.CreateExtractValue(StaticV, ElNum)),
                      B.CreateGEP(Elements, B.getInt32(ElNum)));
      }
    } else {
      Elements = ConstantPointerNull::get(
          PointerType::getUnqual(getValuePtrTy(B.getContext())));
    }

    if (ST->hasName()) {
      StagedV = B.CreateCall(getConstNamedStructFn(getModule()),
                             {C.getType(ST), Elements,
                              ConstantInt::get(getUnsignedIntTy(B.getContext()),
                                               ST->getNumElements())});
    } else {
      StagedV = B.CreateCall(
          getConstStructInContextFn(getModule()),
          {C.getContext(), Elements,
           ConstantInt::get(getUnsignedIntTy(B.getContext()),
                            ST->getNumElements()),
           ConstantInt::get(getBoolTy(B.getContext()), ST->isPacked())});
    }
    break;
  }
  }

  assert((StagedV || StaticV->getType()->isVoidTy()) &&
         "Unsupported static value");
  setStagedValue(V, StagedV);
  return StagedV;
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::setStagedValue(Value *V,
                                                Instruction *StagedV) {
  assert(!StagedValues.count(V) && "Value is already staged");

  // Call stage callbacks for the value.
  for (auto &SCPair: make_range(StageCallbacks.equal_range(V))) {
    SCPair.second(StagedV);
  }
  StageCallbacks.erase(V);

  StagedValues[V] = StagedV;
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::whenStaged(
    Value *V, std::function<void(Instruction *)> Callback) {
  if (Instruction *StagedV = StagedValues.lookup(V)) {
    Callback(StagedV);
  } else {
    StageCallbacks.insert(std::make_pair(V, Callback));
  }
}

template <typename IRBuilder>
void StagedIRBuilder<IRBuilder>::setCalledValue(Instruction *I,
                                                Instruction *V) {
  auto *Call = cast<CallInst>(I);
  assert(Call->getCalledFunction()->getName() == "LLVMBuildCall" &&
         "Not a dynamic call");

  Call->setArgOperand(1, V);
}

} // namespace mix

} // namespace llvm

#endif // LLVM_LIB_TRANSFORMS_MIX_STAGEDIRBUILDER_H
