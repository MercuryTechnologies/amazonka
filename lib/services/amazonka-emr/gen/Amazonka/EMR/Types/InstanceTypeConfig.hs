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
-- Module      : Amazonka.EMR.Types.InstanceTypeConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceTypeConfig where

import qualified Amazonka.Core as Core
import Amazonka.EMR.Types.Configuration
import Amazonka.EMR.Types.EbsConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An instance type configuration for each instance type in an instance
-- fleet, which determines the EC2 instances Amazon EMR attempts to
-- provision to fulfill On-Demand and Spot target capacities. When you use
-- an allocation strategy, you can include a maximum of 30 instance type
-- configurations for a fleet. For more information about how to use an
-- allocation strategy, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-instance-fleet.html Configure Instance Fleets>.
-- Without an allocation strategy, you may specify a maximum of five
-- instance type configurations for a fleet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceTypeConfig' smart constructor.
data InstanceTypeConfig = InstanceTypeConfig'
  { -- | The configuration of Amazon Elastic Block Store (Amazon EBS) attached to
    -- each instance as defined by @InstanceType@.
    ebsConfiguration :: Prelude.Maybe EbsConfiguration,
    -- | The bid price for each EC2 Spot Instance type as defined by
    -- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
    -- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
    -- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPrice :: Prelude.Maybe Prelude.Text,
    -- | The number of units that a provisioned instance of this type provides
    -- toward fulfilling the target capacities defined in InstanceFleetConfig.
    -- This value is 1 for a master instance fleet, and must be 1 or greater
    -- for core and task instance fleets. Defaults to 1 if not specified.
    weightedCapacity :: Prelude.Maybe Prelude.Natural,
    -- | A configuration classification that applies when provisioning cluster
    -- instances, which can include configurations for applications and
    -- software that run on the cluster.
    configurations :: Prelude.Maybe [Configuration],
    -- | The custom AMI ID to use for the instance type.
    customAmiId :: Prelude.Maybe Prelude.Text,
    -- | The bid price, as a percentage of On-Demand price, for each EC2 Spot
    -- Instance as defined by @InstanceType@. Expressed as a number (for
    -- example, 20 specifies 20%). If neither @BidPrice@ nor
    -- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
    -- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
    bidPriceAsPercentageOfOnDemandPrice :: Prelude.Maybe Prelude.Double,
    -- | An EC2 instance type, such as @m3.xlarge@.
    instanceType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceTypeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsConfiguration', 'instanceTypeConfig_ebsConfiguration' - The configuration of Amazon Elastic Block Store (Amazon EBS) attached to
-- each instance as defined by @InstanceType@.
--
-- 'bidPrice', 'instanceTypeConfig_bidPrice' - The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- 'weightedCapacity', 'instanceTypeConfig_weightedCapacity' - The number of units that a provisioned instance of this type provides
-- toward fulfilling the target capacities defined in InstanceFleetConfig.
-- This value is 1 for a master instance fleet, and must be 1 or greater
-- for core and task instance fleets. Defaults to 1 if not specified.
--
-- 'configurations', 'instanceTypeConfig_configurations' - A configuration classification that applies when provisioning cluster
-- instances, which can include configurations for applications and
-- software that run on the cluster.
--
-- 'customAmiId', 'instanceTypeConfig_customAmiId' - The custom AMI ID to use for the instance type.
--
-- 'bidPriceAsPercentageOfOnDemandPrice', 'instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice' - The bid price, as a percentage of On-Demand price, for each EC2 Spot
-- Instance as defined by @InstanceType@. Expressed as a number (for
-- example, 20 specifies 20%). If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
--
-- 'instanceType', 'instanceTypeConfig_instanceType' - An EC2 instance type, such as @m3.xlarge@.
newInstanceTypeConfig ::
  -- | 'instanceType'
  Prelude.Text ->
  InstanceTypeConfig
newInstanceTypeConfig pInstanceType_ =
  InstanceTypeConfig'
    { ebsConfiguration =
        Prelude.Nothing,
      bidPrice = Prelude.Nothing,
      weightedCapacity = Prelude.Nothing,
      configurations = Prelude.Nothing,
      customAmiId = Prelude.Nothing,
      bidPriceAsPercentageOfOnDemandPrice =
        Prelude.Nothing,
      instanceType = pInstanceType_
    }

-- | The configuration of Amazon Elastic Block Store (Amazon EBS) attached to
-- each instance as defined by @InstanceType@.
instanceTypeConfig_ebsConfiguration :: Lens.Lens' InstanceTypeConfig (Prelude.Maybe EbsConfiguration)
instanceTypeConfig_ebsConfiguration = Lens.lens (\InstanceTypeConfig' {ebsConfiguration} -> ebsConfiguration) (\s@InstanceTypeConfig' {} a -> s {ebsConfiguration = a} :: InstanceTypeConfig)

-- | The bid price for each EC2 Spot Instance type as defined by
-- @InstanceType@. Expressed in USD. If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
instanceTypeConfig_bidPrice :: Lens.Lens' InstanceTypeConfig (Prelude.Maybe Prelude.Text)
instanceTypeConfig_bidPrice = Lens.lens (\InstanceTypeConfig' {bidPrice} -> bidPrice) (\s@InstanceTypeConfig' {} a -> s {bidPrice = a} :: InstanceTypeConfig)

-- | The number of units that a provisioned instance of this type provides
-- toward fulfilling the target capacities defined in InstanceFleetConfig.
-- This value is 1 for a master instance fleet, and must be 1 or greater
-- for core and task instance fleets. Defaults to 1 if not specified.
instanceTypeConfig_weightedCapacity :: Lens.Lens' InstanceTypeConfig (Prelude.Maybe Prelude.Natural)
instanceTypeConfig_weightedCapacity = Lens.lens (\InstanceTypeConfig' {weightedCapacity} -> weightedCapacity) (\s@InstanceTypeConfig' {} a -> s {weightedCapacity = a} :: InstanceTypeConfig)

-- | A configuration classification that applies when provisioning cluster
-- instances, which can include configurations for applications and
-- software that run on the cluster.
instanceTypeConfig_configurations :: Lens.Lens' InstanceTypeConfig (Prelude.Maybe [Configuration])
instanceTypeConfig_configurations = Lens.lens (\InstanceTypeConfig' {configurations} -> configurations) (\s@InstanceTypeConfig' {} a -> s {configurations = a} :: InstanceTypeConfig) Prelude.. Lens.mapping Lens.coerced

-- | The custom AMI ID to use for the instance type.
instanceTypeConfig_customAmiId :: Lens.Lens' InstanceTypeConfig (Prelude.Maybe Prelude.Text)
instanceTypeConfig_customAmiId = Lens.lens (\InstanceTypeConfig' {customAmiId} -> customAmiId) (\s@InstanceTypeConfig' {} a -> s {customAmiId = a} :: InstanceTypeConfig)

-- | The bid price, as a percentage of On-Demand price, for each EC2 Spot
-- Instance as defined by @InstanceType@. Expressed as a number (for
-- example, 20 specifies 20%). If neither @BidPrice@ nor
-- @BidPriceAsPercentageOfOnDemandPrice@ is provided,
-- @BidPriceAsPercentageOfOnDemandPrice@ defaults to 100%.
instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice :: Lens.Lens' InstanceTypeConfig (Prelude.Maybe Prelude.Double)
instanceTypeConfig_bidPriceAsPercentageOfOnDemandPrice = Lens.lens (\InstanceTypeConfig' {bidPriceAsPercentageOfOnDemandPrice} -> bidPriceAsPercentageOfOnDemandPrice) (\s@InstanceTypeConfig' {} a -> s {bidPriceAsPercentageOfOnDemandPrice = a} :: InstanceTypeConfig)

-- | An EC2 instance type, such as @m3.xlarge@.
instanceTypeConfig_instanceType :: Lens.Lens' InstanceTypeConfig Prelude.Text
instanceTypeConfig_instanceType = Lens.lens (\InstanceTypeConfig' {instanceType} -> instanceType) (\s@InstanceTypeConfig' {} a -> s {instanceType = a} :: InstanceTypeConfig)

instance Prelude.Hashable InstanceTypeConfig where
  hashWithSalt salt' InstanceTypeConfig' {..} =
    salt' `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` bidPriceAsPercentageOfOnDemandPrice
      `Prelude.hashWithSalt` customAmiId
      `Prelude.hashWithSalt` configurations
      `Prelude.hashWithSalt` weightedCapacity
      `Prelude.hashWithSalt` bidPrice
      `Prelude.hashWithSalt` ebsConfiguration

instance Prelude.NFData InstanceTypeConfig where
  rnf InstanceTypeConfig' {..} =
    Prelude.rnf ebsConfiguration
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf bidPriceAsPercentageOfOnDemandPrice
      `Prelude.seq` Prelude.rnf customAmiId
      `Prelude.seq` Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf weightedCapacity
      `Prelude.seq` Prelude.rnf bidPrice

instance Core.ToJSON InstanceTypeConfig where
  toJSON InstanceTypeConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EbsConfiguration" Core..=)
              Prelude.<$> ebsConfiguration,
            ("BidPrice" Core..=) Prelude.<$> bidPrice,
            ("WeightedCapacity" Core..=)
              Prelude.<$> weightedCapacity,
            ("Configurations" Core..=)
              Prelude.<$> configurations,
            ("CustomAmiId" Core..=) Prelude.<$> customAmiId,
            ("BidPriceAsPercentageOfOnDemandPrice" Core..=)
              Prelude.<$> bidPriceAsPercentageOfOnDemandPrice,
            Prelude.Just ("InstanceType" Core..= instanceType)
          ]
      )
