{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.Types.InstanceTypeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceTypeInfo where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.BootModeType
import Amazonka.EC2.Types.EbsInfo
import Amazonka.EC2.Types.FpgaInfo
import Amazonka.EC2.Types.GpuInfo
import Amazonka.EC2.Types.InferenceAcceleratorInfo
import Amazonka.EC2.Types.InstanceStorageInfo
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.InstanceTypeHypervisor
import Amazonka.EC2.Types.MemoryInfo
import Amazonka.EC2.Types.NetworkInfo
import Amazonka.EC2.Types.PlacementGroupInfo
import Amazonka.EC2.Types.ProcessorInfo
import Amazonka.EC2.Types.RootDeviceType
import Amazonka.EC2.Types.UsageClassType
import Amazonka.EC2.Types.VCpuInfo
import Amazonka.EC2.Types.VirtualizationType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the instance type.
--
-- /See:/ 'newInstanceTypeInfo' smart constructor.
data InstanceTypeInfo = InstanceTypeInfo'
  { -- | The hypervisor for the instance type.
    hypervisor :: Prelude.Maybe InstanceTypeHypervisor,
    -- | Indicates whether the instance type is current generation.
    currentGeneration :: Prelude.Maybe Prelude.Bool,
    -- | Describes the memory for the instance type.
    memoryInfo :: Prelude.Maybe MemoryInfo,
    -- | Describes the placement group settings for the instance type.
    placementGroupInfo :: Prelude.Maybe PlacementGroupInfo,
    -- | The supported boot modes. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
    -- in the /Amazon EC2 User Guide/.
    supportedBootModes :: Prelude.Maybe [BootModeType],
    -- | The supported root device types.
    supportedRootDeviceTypes :: Prelude.Maybe [RootDeviceType],
    -- | Indicates whether the instance type is offered for spot or On-Demand.
    supportedUsageClasses :: Prelude.Maybe [UsageClassType],
    -- | Indicates whether instance storage is supported.
    instanceStorageSupported :: Prelude.Maybe Prelude.Bool,
    -- | Describes the FPGA accelerator settings for the instance type.
    fpgaInfo :: Prelude.Maybe FpgaInfo,
    -- | Indicates whether the instance type is a burstable performance instance
    -- type.
    burstablePerformanceSupported :: Prelude.Maybe Prelude.Bool,
    -- | The instance type. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
    -- in the /Amazon EC2 User Guide/.
    instanceType :: Prelude.Maybe InstanceType,
    -- | Describes the GPU accelerator settings for the instance type.
    gpuInfo :: Prelude.Maybe GpuInfo,
    -- | The supported virtualization types.
    supportedVirtualizationTypes :: Prelude.Maybe [VirtualizationType],
    -- | Describes the Amazon EBS settings for the instance type.
    ebsInfo :: Prelude.Maybe EbsInfo,
    -- | Indicates whether auto recovery is supported.
    autoRecoverySupported :: Prelude.Maybe Prelude.Bool,
    -- | Describes the Inference accelerator settings for the instance type.
    inferenceAcceleratorInfo :: Prelude.Maybe InferenceAcceleratorInfo,
    -- | Indicates whether the instance is a bare metal instance type.
    bareMetal :: Prelude.Maybe Prelude.Bool,
    -- | Describes the network settings for the instance type.
    networkInfo :: Prelude.Maybe NetworkInfo,
    -- | Describes the processor.
    processorInfo :: Prelude.Maybe ProcessorInfo,
    -- | Indicates whether the instance type is eligible for the free tier.
    freeTierEligible :: Prelude.Maybe Prelude.Bool,
    -- | Describes the vCPU configurations for the instance type.
    vCpuInfo :: Prelude.Maybe VCpuInfo,
    -- | Describes the instance storage for the instance type.
    instanceStorageInfo :: Prelude.Maybe InstanceStorageInfo,
    -- | Indicates whether Dedicated Hosts are supported on the instance type.
    dedicatedHostsSupported :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether On-Demand hibernation is supported.
    hibernationSupported :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceTypeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisor', 'instanceTypeInfo_hypervisor' - The hypervisor for the instance type.
--
-- 'currentGeneration', 'instanceTypeInfo_currentGeneration' - Indicates whether the instance type is current generation.
--
-- 'memoryInfo', 'instanceTypeInfo_memoryInfo' - Describes the memory for the instance type.
--
-- 'placementGroupInfo', 'instanceTypeInfo_placementGroupInfo' - Describes the placement group settings for the instance type.
--
-- 'supportedBootModes', 'instanceTypeInfo_supportedBootModes' - The supported boot modes. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon EC2 User Guide/.
--
-- 'supportedRootDeviceTypes', 'instanceTypeInfo_supportedRootDeviceTypes' - The supported root device types.
--
-- 'supportedUsageClasses', 'instanceTypeInfo_supportedUsageClasses' - Indicates whether the instance type is offered for spot or On-Demand.
--
-- 'instanceStorageSupported', 'instanceTypeInfo_instanceStorageSupported' - Indicates whether instance storage is supported.
--
-- 'fpgaInfo', 'instanceTypeInfo_fpgaInfo' - Describes the FPGA accelerator settings for the instance type.
--
-- 'burstablePerformanceSupported', 'instanceTypeInfo_burstablePerformanceSupported' - Indicates whether the instance type is a burstable performance instance
-- type.
--
-- 'instanceType', 'instanceTypeInfo_instanceType' - The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
--
-- 'gpuInfo', 'instanceTypeInfo_gpuInfo' - Describes the GPU accelerator settings for the instance type.
--
-- 'supportedVirtualizationTypes', 'instanceTypeInfo_supportedVirtualizationTypes' - The supported virtualization types.
--
-- 'ebsInfo', 'instanceTypeInfo_ebsInfo' - Describes the Amazon EBS settings for the instance type.
--
-- 'autoRecoverySupported', 'instanceTypeInfo_autoRecoverySupported' - Indicates whether auto recovery is supported.
--
-- 'inferenceAcceleratorInfo', 'instanceTypeInfo_inferenceAcceleratorInfo' - Describes the Inference accelerator settings for the instance type.
--
-- 'bareMetal', 'instanceTypeInfo_bareMetal' - Indicates whether the instance is a bare metal instance type.
--
-- 'networkInfo', 'instanceTypeInfo_networkInfo' - Describes the network settings for the instance type.
--
-- 'processorInfo', 'instanceTypeInfo_processorInfo' - Describes the processor.
--
-- 'freeTierEligible', 'instanceTypeInfo_freeTierEligible' - Indicates whether the instance type is eligible for the free tier.
--
-- 'vCpuInfo', 'instanceTypeInfo_vCpuInfo' - Describes the vCPU configurations for the instance type.
--
-- 'instanceStorageInfo', 'instanceTypeInfo_instanceStorageInfo' - Describes the instance storage for the instance type.
--
-- 'dedicatedHostsSupported', 'instanceTypeInfo_dedicatedHostsSupported' - Indicates whether Dedicated Hosts are supported on the instance type.
--
-- 'hibernationSupported', 'instanceTypeInfo_hibernationSupported' - Indicates whether On-Demand hibernation is supported.
newInstanceTypeInfo ::
  InstanceTypeInfo
newInstanceTypeInfo =
  InstanceTypeInfo'
    { hypervisor = Prelude.Nothing,
      currentGeneration = Prelude.Nothing,
      memoryInfo = Prelude.Nothing,
      placementGroupInfo = Prelude.Nothing,
      supportedBootModes = Prelude.Nothing,
      supportedRootDeviceTypes = Prelude.Nothing,
      supportedUsageClasses = Prelude.Nothing,
      instanceStorageSupported = Prelude.Nothing,
      fpgaInfo = Prelude.Nothing,
      burstablePerformanceSupported = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      gpuInfo = Prelude.Nothing,
      supportedVirtualizationTypes = Prelude.Nothing,
      ebsInfo = Prelude.Nothing,
      autoRecoverySupported = Prelude.Nothing,
      inferenceAcceleratorInfo = Prelude.Nothing,
      bareMetal = Prelude.Nothing,
      networkInfo = Prelude.Nothing,
      processorInfo = Prelude.Nothing,
      freeTierEligible = Prelude.Nothing,
      vCpuInfo = Prelude.Nothing,
      instanceStorageInfo = Prelude.Nothing,
      dedicatedHostsSupported = Prelude.Nothing,
      hibernationSupported = Prelude.Nothing
    }

-- | The hypervisor for the instance type.
instanceTypeInfo_hypervisor :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe InstanceTypeHypervisor)
instanceTypeInfo_hypervisor = Lens.lens (\InstanceTypeInfo' {hypervisor} -> hypervisor) (\s@InstanceTypeInfo' {} a -> s {hypervisor = a} :: InstanceTypeInfo)

-- | Indicates whether the instance type is current generation.
instanceTypeInfo_currentGeneration :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe Prelude.Bool)
instanceTypeInfo_currentGeneration = Lens.lens (\InstanceTypeInfo' {currentGeneration} -> currentGeneration) (\s@InstanceTypeInfo' {} a -> s {currentGeneration = a} :: InstanceTypeInfo)

-- | Describes the memory for the instance type.
instanceTypeInfo_memoryInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe MemoryInfo)
instanceTypeInfo_memoryInfo = Lens.lens (\InstanceTypeInfo' {memoryInfo} -> memoryInfo) (\s@InstanceTypeInfo' {} a -> s {memoryInfo = a} :: InstanceTypeInfo)

-- | Describes the placement group settings for the instance type.
instanceTypeInfo_placementGroupInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe PlacementGroupInfo)
instanceTypeInfo_placementGroupInfo = Lens.lens (\InstanceTypeInfo' {placementGroupInfo} -> placementGroupInfo) (\s@InstanceTypeInfo' {} a -> s {placementGroupInfo = a} :: InstanceTypeInfo)

-- | The supported boot modes. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon EC2 User Guide/.
instanceTypeInfo_supportedBootModes :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe [BootModeType])
instanceTypeInfo_supportedBootModes = Lens.lens (\InstanceTypeInfo' {supportedBootModes} -> supportedBootModes) (\s@InstanceTypeInfo' {} a -> s {supportedBootModes = a} :: InstanceTypeInfo) Prelude.. Lens.mapping Lens.coerced

-- | The supported root device types.
instanceTypeInfo_supportedRootDeviceTypes :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe [RootDeviceType])
instanceTypeInfo_supportedRootDeviceTypes = Lens.lens (\InstanceTypeInfo' {supportedRootDeviceTypes} -> supportedRootDeviceTypes) (\s@InstanceTypeInfo' {} a -> s {supportedRootDeviceTypes = a} :: InstanceTypeInfo) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the instance type is offered for spot or On-Demand.
instanceTypeInfo_supportedUsageClasses :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe [UsageClassType])
instanceTypeInfo_supportedUsageClasses = Lens.lens (\InstanceTypeInfo' {supportedUsageClasses} -> supportedUsageClasses) (\s@InstanceTypeInfo' {} a -> s {supportedUsageClasses = a} :: InstanceTypeInfo) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether instance storage is supported.
instanceTypeInfo_instanceStorageSupported :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe Prelude.Bool)
instanceTypeInfo_instanceStorageSupported = Lens.lens (\InstanceTypeInfo' {instanceStorageSupported} -> instanceStorageSupported) (\s@InstanceTypeInfo' {} a -> s {instanceStorageSupported = a} :: InstanceTypeInfo)

-- | Describes the FPGA accelerator settings for the instance type.
instanceTypeInfo_fpgaInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe FpgaInfo)
instanceTypeInfo_fpgaInfo = Lens.lens (\InstanceTypeInfo' {fpgaInfo} -> fpgaInfo) (\s@InstanceTypeInfo' {} a -> s {fpgaInfo = a} :: InstanceTypeInfo)

-- | Indicates whether the instance type is a burstable performance instance
-- type.
instanceTypeInfo_burstablePerformanceSupported :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe Prelude.Bool)
instanceTypeInfo_burstablePerformanceSupported = Lens.lens (\InstanceTypeInfo' {burstablePerformanceSupported} -> burstablePerformanceSupported) (\s@InstanceTypeInfo' {} a -> s {burstablePerformanceSupported = a} :: InstanceTypeInfo)

-- | The instance type. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types>
-- in the /Amazon EC2 User Guide/.
instanceTypeInfo_instanceType :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe InstanceType)
instanceTypeInfo_instanceType = Lens.lens (\InstanceTypeInfo' {instanceType} -> instanceType) (\s@InstanceTypeInfo' {} a -> s {instanceType = a} :: InstanceTypeInfo)

-- | Describes the GPU accelerator settings for the instance type.
instanceTypeInfo_gpuInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe GpuInfo)
instanceTypeInfo_gpuInfo = Lens.lens (\InstanceTypeInfo' {gpuInfo} -> gpuInfo) (\s@InstanceTypeInfo' {} a -> s {gpuInfo = a} :: InstanceTypeInfo)

-- | The supported virtualization types.
instanceTypeInfo_supportedVirtualizationTypes :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe [VirtualizationType])
instanceTypeInfo_supportedVirtualizationTypes = Lens.lens (\InstanceTypeInfo' {supportedVirtualizationTypes} -> supportedVirtualizationTypes) (\s@InstanceTypeInfo' {} a -> s {supportedVirtualizationTypes = a} :: InstanceTypeInfo) Prelude.. Lens.mapping Lens.coerced

-- | Describes the Amazon EBS settings for the instance type.
instanceTypeInfo_ebsInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe EbsInfo)
instanceTypeInfo_ebsInfo = Lens.lens (\InstanceTypeInfo' {ebsInfo} -> ebsInfo) (\s@InstanceTypeInfo' {} a -> s {ebsInfo = a} :: InstanceTypeInfo)

-- | Indicates whether auto recovery is supported.
instanceTypeInfo_autoRecoverySupported :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe Prelude.Bool)
instanceTypeInfo_autoRecoverySupported = Lens.lens (\InstanceTypeInfo' {autoRecoverySupported} -> autoRecoverySupported) (\s@InstanceTypeInfo' {} a -> s {autoRecoverySupported = a} :: InstanceTypeInfo)

-- | Describes the Inference accelerator settings for the instance type.
instanceTypeInfo_inferenceAcceleratorInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe InferenceAcceleratorInfo)
instanceTypeInfo_inferenceAcceleratorInfo = Lens.lens (\InstanceTypeInfo' {inferenceAcceleratorInfo} -> inferenceAcceleratorInfo) (\s@InstanceTypeInfo' {} a -> s {inferenceAcceleratorInfo = a} :: InstanceTypeInfo)

-- | Indicates whether the instance is a bare metal instance type.
instanceTypeInfo_bareMetal :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe Prelude.Bool)
instanceTypeInfo_bareMetal = Lens.lens (\InstanceTypeInfo' {bareMetal} -> bareMetal) (\s@InstanceTypeInfo' {} a -> s {bareMetal = a} :: InstanceTypeInfo)

-- | Describes the network settings for the instance type.
instanceTypeInfo_networkInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe NetworkInfo)
instanceTypeInfo_networkInfo = Lens.lens (\InstanceTypeInfo' {networkInfo} -> networkInfo) (\s@InstanceTypeInfo' {} a -> s {networkInfo = a} :: InstanceTypeInfo)

-- | Describes the processor.
instanceTypeInfo_processorInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe ProcessorInfo)
instanceTypeInfo_processorInfo = Lens.lens (\InstanceTypeInfo' {processorInfo} -> processorInfo) (\s@InstanceTypeInfo' {} a -> s {processorInfo = a} :: InstanceTypeInfo)

-- | Indicates whether the instance type is eligible for the free tier.
instanceTypeInfo_freeTierEligible :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe Prelude.Bool)
instanceTypeInfo_freeTierEligible = Lens.lens (\InstanceTypeInfo' {freeTierEligible} -> freeTierEligible) (\s@InstanceTypeInfo' {} a -> s {freeTierEligible = a} :: InstanceTypeInfo)

-- | Describes the vCPU configurations for the instance type.
instanceTypeInfo_vCpuInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe VCpuInfo)
instanceTypeInfo_vCpuInfo = Lens.lens (\InstanceTypeInfo' {vCpuInfo} -> vCpuInfo) (\s@InstanceTypeInfo' {} a -> s {vCpuInfo = a} :: InstanceTypeInfo)

-- | Describes the instance storage for the instance type.
instanceTypeInfo_instanceStorageInfo :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe InstanceStorageInfo)
instanceTypeInfo_instanceStorageInfo = Lens.lens (\InstanceTypeInfo' {instanceStorageInfo} -> instanceStorageInfo) (\s@InstanceTypeInfo' {} a -> s {instanceStorageInfo = a} :: InstanceTypeInfo)

-- | Indicates whether Dedicated Hosts are supported on the instance type.
instanceTypeInfo_dedicatedHostsSupported :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe Prelude.Bool)
instanceTypeInfo_dedicatedHostsSupported = Lens.lens (\InstanceTypeInfo' {dedicatedHostsSupported} -> dedicatedHostsSupported) (\s@InstanceTypeInfo' {} a -> s {dedicatedHostsSupported = a} :: InstanceTypeInfo)

-- | Indicates whether On-Demand hibernation is supported.
instanceTypeInfo_hibernationSupported :: Lens.Lens' InstanceTypeInfo (Prelude.Maybe Prelude.Bool)
instanceTypeInfo_hibernationSupported = Lens.lens (\InstanceTypeInfo' {hibernationSupported} -> hibernationSupported) (\s@InstanceTypeInfo' {} a -> s {hibernationSupported = a} :: InstanceTypeInfo)

instance Core.FromXML InstanceTypeInfo where
  parseXML x =
    InstanceTypeInfo'
      Prelude.<$> (x Core..@? "hypervisor")
      Prelude.<*> (x Core..@? "currentGeneration")
      Prelude.<*> (x Core..@? "memoryInfo")
      Prelude.<*> (x Core..@? "placementGroupInfo")
      Prelude.<*> ( x Core..@? "supportedBootModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "supportedRootDeviceTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "supportedUsageClasses"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "instanceStorageSupported")
      Prelude.<*> (x Core..@? "fpgaInfo")
      Prelude.<*> (x Core..@? "burstablePerformanceSupported")
      Prelude.<*> (x Core..@? "instanceType")
      Prelude.<*> (x Core..@? "gpuInfo")
      Prelude.<*> ( x Core..@? "supportedVirtualizationTypes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "ebsInfo")
      Prelude.<*> (x Core..@? "autoRecoverySupported")
      Prelude.<*> (x Core..@? "inferenceAcceleratorInfo")
      Prelude.<*> (x Core..@? "bareMetal")
      Prelude.<*> (x Core..@? "networkInfo")
      Prelude.<*> (x Core..@? "processorInfo")
      Prelude.<*> (x Core..@? "freeTierEligible")
      Prelude.<*> (x Core..@? "vCpuInfo")
      Prelude.<*> (x Core..@? "instanceStorageInfo")
      Prelude.<*> (x Core..@? "dedicatedHostsSupported")
      Prelude.<*> (x Core..@? "hibernationSupported")

instance Prelude.Hashable InstanceTypeInfo where
  hashWithSalt salt' InstanceTypeInfo' {..} =
    salt' `Prelude.hashWithSalt` hibernationSupported
      `Prelude.hashWithSalt` dedicatedHostsSupported
      `Prelude.hashWithSalt` instanceStorageInfo
      `Prelude.hashWithSalt` vCpuInfo
      `Prelude.hashWithSalt` freeTierEligible
      `Prelude.hashWithSalt` processorInfo
      `Prelude.hashWithSalt` networkInfo
      `Prelude.hashWithSalt` bareMetal
      `Prelude.hashWithSalt` inferenceAcceleratorInfo
      `Prelude.hashWithSalt` autoRecoverySupported
      `Prelude.hashWithSalt` ebsInfo
      `Prelude.hashWithSalt` supportedVirtualizationTypes
      `Prelude.hashWithSalt` gpuInfo
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` burstablePerformanceSupported
      `Prelude.hashWithSalt` fpgaInfo
      `Prelude.hashWithSalt` instanceStorageSupported
      `Prelude.hashWithSalt` supportedUsageClasses
      `Prelude.hashWithSalt` supportedRootDeviceTypes
      `Prelude.hashWithSalt` supportedBootModes
      `Prelude.hashWithSalt` placementGroupInfo
      `Prelude.hashWithSalt` memoryInfo
      `Prelude.hashWithSalt` currentGeneration
      `Prelude.hashWithSalt` hypervisor

instance Prelude.NFData InstanceTypeInfo where
  rnf InstanceTypeInfo' {..} =
    Prelude.rnf hypervisor
      `Prelude.seq` Prelude.rnf hibernationSupported
      `Prelude.seq` Prelude.rnf dedicatedHostsSupported
      `Prelude.seq` Prelude.rnf instanceStorageInfo
      `Prelude.seq` Prelude.rnf vCpuInfo
      `Prelude.seq` Prelude.rnf freeTierEligible
      `Prelude.seq` Prelude.rnf processorInfo
      `Prelude.seq` Prelude.rnf networkInfo
      `Prelude.seq` Prelude.rnf bareMetal
      `Prelude.seq` Prelude.rnf inferenceAcceleratorInfo
      `Prelude.seq` Prelude.rnf autoRecoverySupported
      `Prelude.seq` Prelude.rnf ebsInfo
      `Prelude.seq` Prelude.rnf supportedVirtualizationTypes
      `Prelude.seq` Prelude.rnf gpuInfo
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf burstablePerformanceSupported
      `Prelude.seq` Prelude.rnf fpgaInfo
      `Prelude.seq` Prelude.rnf instanceStorageSupported
      `Prelude.seq` Prelude.rnf supportedUsageClasses
      `Prelude.seq` Prelude.rnf supportedRootDeviceTypes
      `Prelude.seq` Prelude.rnf supportedBootModes
      `Prelude.seq` Prelude.rnf placementGroupInfo
      `Prelude.seq` Prelude.rnf memoryInfo
      `Prelude.seq` Prelude.rnf currentGeneration
