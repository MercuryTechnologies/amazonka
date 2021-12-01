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
-- Module      : Amazonka.EMR.Types.InstanceGroupConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceGroupConfig where

import qualified Amazonka.Core as Core
import Amazonka.EMR.Types.AutoScalingPolicy
import Amazonka.EMR.Types.Configuration
import Amazonka.EMR.Types.EbsConfiguration
import Amazonka.EMR.Types.InstanceRoleType
import Amazonka.EMR.Types.MarketType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration defining a new instance group.
--
-- /See:/ 'newInstanceGroupConfig' smart constructor.
data InstanceGroupConfig = InstanceGroupConfig'
  { -- | EBS configurations that will be attached to each EC2 instance in the
    -- instance group.
    ebsConfiguration :: Prelude.Maybe EbsConfiguration,
    -- | If specified, indicates that the instance group uses Spot Instances.
    -- This is the maximum price you are willing to pay for Spot Instances.
    -- Specify @OnDemandPrice@ to set the amount equal to the On-Demand price,
    -- or specify an amount in USD.
    bidPrice :: Prelude.Maybe Prelude.Text,
    -- | Amazon EMR releases 4.x or later.
    --
    -- The list of configurations supplied for an EMR cluster instance group.
    -- You can specify a separate configuration for each instance group
    -- (master, core, and task).
    configurations :: Prelude.Maybe [Configuration],
    -- | The custom AMI ID to use for the provisioned instance group.
    customAmiId :: Prelude.Maybe Prelude.Text,
    -- | Market type of the EC2 instances used to create a cluster node.
    market :: Prelude.Maybe MarketType,
    -- | Friendly name given to the instance group.
    name :: Prelude.Maybe Prelude.Text,
    -- | An automatic scaling policy for a core instance group or task instance
    -- group in an Amazon EMR cluster. The automatic scaling policy defines how
    -- an instance group dynamically adds and terminates EC2 instances in
    -- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
    autoScalingPolicy :: Prelude.Maybe AutoScalingPolicy,
    -- | The role of the instance group in the cluster.
    instanceRole :: InstanceRoleType,
    -- | The EC2 instance type for all instances in the instance group.
    instanceType :: Prelude.Text,
    -- | Target number of instances for the instance group.
    instanceCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceGroupConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsConfiguration', 'instanceGroupConfig_ebsConfiguration' - EBS configurations that will be attached to each EC2 instance in the
-- instance group.
--
-- 'bidPrice', 'instanceGroupConfig_bidPrice' - If specified, indicates that the instance group uses Spot Instances.
-- This is the maximum price you are willing to pay for Spot Instances.
-- Specify @OnDemandPrice@ to set the amount equal to the On-Demand price,
-- or specify an amount in USD.
--
-- 'configurations', 'instanceGroupConfig_configurations' - Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an EMR cluster instance group.
-- You can specify a separate configuration for each instance group
-- (master, core, and task).
--
-- 'customAmiId', 'instanceGroupConfig_customAmiId' - The custom AMI ID to use for the provisioned instance group.
--
-- 'market', 'instanceGroupConfig_market' - Market type of the EC2 instances used to create a cluster node.
--
-- 'name', 'instanceGroupConfig_name' - Friendly name given to the instance group.
--
-- 'autoScalingPolicy', 'instanceGroupConfig_autoScalingPolicy' - An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
--
-- 'instanceRole', 'instanceGroupConfig_instanceRole' - The role of the instance group in the cluster.
--
-- 'instanceType', 'instanceGroupConfig_instanceType' - The EC2 instance type for all instances in the instance group.
--
-- 'instanceCount', 'instanceGroupConfig_instanceCount' - Target number of instances for the instance group.
newInstanceGroupConfig ::
  -- | 'instanceRole'
  InstanceRoleType ->
  -- | 'instanceType'
  Prelude.Text ->
  -- | 'instanceCount'
  Prelude.Int ->
  InstanceGroupConfig
newInstanceGroupConfig
  pInstanceRole_
  pInstanceType_
  pInstanceCount_ =
    InstanceGroupConfig'
      { ebsConfiguration =
          Prelude.Nothing,
        bidPrice = Prelude.Nothing,
        configurations = Prelude.Nothing,
        customAmiId = Prelude.Nothing,
        market = Prelude.Nothing,
        name = Prelude.Nothing,
        autoScalingPolicy = Prelude.Nothing,
        instanceRole = pInstanceRole_,
        instanceType = pInstanceType_,
        instanceCount = pInstanceCount_
      }

-- | EBS configurations that will be attached to each EC2 instance in the
-- instance group.
instanceGroupConfig_ebsConfiguration :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe EbsConfiguration)
instanceGroupConfig_ebsConfiguration = Lens.lens (\InstanceGroupConfig' {ebsConfiguration} -> ebsConfiguration) (\s@InstanceGroupConfig' {} a -> s {ebsConfiguration = a} :: InstanceGroupConfig)

-- | If specified, indicates that the instance group uses Spot Instances.
-- This is the maximum price you are willing to pay for Spot Instances.
-- Specify @OnDemandPrice@ to set the amount equal to the On-Demand price,
-- or specify an amount in USD.
instanceGroupConfig_bidPrice :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe Prelude.Text)
instanceGroupConfig_bidPrice = Lens.lens (\InstanceGroupConfig' {bidPrice} -> bidPrice) (\s@InstanceGroupConfig' {} a -> s {bidPrice = a} :: InstanceGroupConfig)

-- | Amazon EMR releases 4.x or later.
--
-- The list of configurations supplied for an EMR cluster instance group.
-- You can specify a separate configuration for each instance group
-- (master, core, and task).
instanceGroupConfig_configurations :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe [Configuration])
instanceGroupConfig_configurations = Lens.lens (\InstanceGroupConfig' {configurations} -> configurations) (\s@InstanceGroupConfig' {} a -> s {configurations = a} :: InstanceGroupConfig) Prelude.. Lens.mapping Lens.coerced

-- | The custom AMI ID to use for the provisioned instance group.
instanceGroupConfig_customAmiId :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe Prelude.Text)
instanceGroupConfig_customAmiId = Lens.lens (\InstanceGroupConfig' {customAmiId} -> customAmiId) (\s@InstanceGroupConfig' {} a -> s {customAmiId = a} :: InstanceGroupConfig)

-- | Market type of the EC2 instances used to create a cluster node.
instanceGroupConfig_market :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe MarketType)
instanceGroupConfig_market = Lens.lens (\InstanceGroupConfig' {market} -> market) (\s@InstanceGroupConfig' {} a -> s {market = a} :: InstanceGroupConfig)

-- | Friendly name given to the instance group.
instanceGroupConfig_name :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe Prelude.Text)
instanceGroupConfig_name = Lens.lens (\InstanceGroupConfig' {name} -> name) (\s@InstanceGroupConfig' {} a -> s {name = a} :: InstanceGroupConfig)

-- | An automatic scaling policy for a core instance group or task instance
-- group in an Amazon EMR cluster. The automatic scaling policy defines how
-- an instance group dynamically adds and terminates EC2 instances in
-- response to the value of a CloudWatch metric. See PutAutoScalingPolicy.
instanceGroupConfig_autoScalingPolicy :: Lens.Lens' InstanceGroupConfig (Prelude.Maybe AutoScalingPolicy)
instanceGroupConfig_autoScalingPolicy = Lens.lens (\InstanceGroupConfig' {autoScalingPolicy} -> autoScalingPolicy) (\s@InstanceGroupConfig' {} a -> s {autoScalingPolicy = a} :: InstanceGroupConfig)

-- | The role of the instance group in the cluster.
instanceGroupConfig_instanceRole :: Lens.Lens' InstanceGroupConfig InstanceRoleType
instanceGroupConfig_instanceRole = Lens.lens (\InstanceGroupConfig' {instanceRole} -> instanceRole) (\s@InstanceGroupConfig' {} a -> s {instanceRole = a} :: InstanceGroupConfig)

-- | The EC2 instance type for all instances in the instance group.
instanceGroupConfig_instanceType :: Lens.Lens' InstanceGroupConfig Prelude.Text
instanceGroupConfig_instanceType = Lens.lens (\InstanceGroupConfig' {instanceType} -> instanceType) (\s@InstanceGroupConfig' {} a -> s {instanceType = a} :: InstanceGroupConfig)

-- | Target number of instances for the instance group.
instanceGroupConfig_instanceCount :: Lens.Lens' InstanceGroupConfig Prelude.Int
instanceGroupConfig_instanceCount = Lens.lens (\InstanceGroupConfig' {instanceCount} -> instanceCount) (\s@InstanceGroupConfig' {} a -> s {instanceCount = a} :: InstanceGroupConfig)

instance Prelude.Hashable InstanceGroupConfig where
  hashWithSalt salt' InstanceGroupConfig' {..} =
    salt' `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` instanceRole
      `Prelude.hashWithSalt` autoScalingPolicy
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` market
      `Prelude.hashWithSalt` customAmiId
      `Prelude.hashWithSalt` configurations
      `Prelude.hashWithSalt` bidPrice
      `Prelude.hashWithSalt` ebsConfiguration

instance Prelude.NFData InstanceGroupConfig where
  rnf InstanceGroupConfig' {..} =
    Prelude.rnf ebsConfiguration
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf instanceRole
      `Prelude.seq` Prelude.rnf autoScalingPolicy
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf market
      `Prelude.seq` Prelude.rnf customAmiId
      `Prelude.seq` Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf bidPrice

instance Core.ToJSON InstanceGroupConfig where
  toJSON InstanceGroupConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EbsConfiguration" Core..=)
              Prelude.<$> ebsConfiguration,
            ("BidPrice" Core..=) Prelude.<$> bidPrice,
            ("Configurations" Core..=)
              Prelude.<$> configurations,
            ("CustomAmiId" Core..=) Prelude.<$> customAmiId,
            ("Market" Core..=) Prelude.<$> market,
            ("Name" Core..=) Prelude.<$> name,
            ("AutoScalingPolicy" Core..=)
              Prelude.<$> autoScalingPolicy,
            Prelude.Just ("InstanceRole" Core..= instanceRole),
            Prelude.Just ("InstanceType" Core..= instanceType),
            Prelude.Just
              ("InstanceCount" Core..= instanceCount)
          ]
      )
