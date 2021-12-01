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
-- Module      : Amazonka.EC2.Types.Placement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Placement where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tenancy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the placement of an instance.
--
-- /See:/ 'newPlacement' smart constructor.
data Placement = Placement'
  { -- | The affinity setting for the instance on the Dedicated Host. This
    -- parameter is not supported for the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
    -- command.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    affinity :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Dedicated Host on which the instance resides. This
    -- parameter is not supported for the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
    -- command.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    hostId :: Prelude.Maybe Prelude.Text,
    -- | The number of the partition the instance is in. Valid only if the
    -- placement group strategy is set to @partition@.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    partitionNumber :: Prelude.Maybe Prelude.Int,
    -- | Reserved for future use.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    spreadDomain :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone of the instance.
    --
    -- If not specified, an Availability Zone will be automatically chosen for
    -- you based on the load balancing criteria for the Region.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance (if the instance is running in a VPC). An
    -- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
    -- The @host@ tenancy is not supported for the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
    -- command.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    --
    -- T3 instances that use the @unlimited@ CPU credit option do not support
    -- @host@ tenancy.
    tenancy :: Prelude.Maybe Tenancy,
    -- | The name of the placement group the instance is in.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the host resource group in which to launch the instances. If
    -- you specify a host resource group ARN, omit the __Tenancy__ parameter or
    -- set it to @host@.
    --
    -- This parameter is not supported by
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
    hostResourceGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Placement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affinity', 'placement_affinity' - The affinity setting for the instance on the Dedicated Host. This
-- parameter is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'hostId', 'placement_hostId' - The ID of the Dedicated Host on which the instance resides. This
-- parameter is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'partitionNumber', 'placement_partitionNumber' - The number of the partition the instance is in. Valid only if the
-- placement group strategy is set to @partition@.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'spreadDomain', 'placement_spreadDomain' - Reserved for future use.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'availabilityZone', 'placement_availabilityZone' - The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for
-- you based on the load balancing criteria for the Region.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- 'tenancy', 'placement_tenancy' - The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
-- The @host@ tenancy is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- T3 instances that use the @unlimited@ CPU credit option do not support
-- @host@ tenancy.
--
-- 'groupName', 'placement_groupName' - The name of the placement group the instance is in.
--
-- 'hostResourceGroupArn', 'placement_hostResourceGroupArn' - The ARN of the host resource group in which to launch the instances. If
-- you specify a host resource group ARN, omit the __Tenancy__ parameter or
-- set it to @host@.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
newPlacement ::
  Placement
newPlacement =
  Placement'
    { affinity = Prelude.Nothing,
      hostId = Prelude.Nothing,
      partitionNumber = Prelude.Nothing,
      spreadDomain = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      tenancy = Prelude.Nothing,
      groupName = Prelude.Nothing,
      hostResourceGroupArn = Prelude.Nothing
    }

-- | The affinity setting for the instance on the Dedicated Host. This
-- parameter is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_affinity :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_affinity = Lens.lens (\Placement' {affinity} -> affinity) (\s@Placement' {} a -> s {affinity = a} :: Placement)

-- | The ID of the Dedicated Host on which the instance resides. This
-- parameter is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_hostId :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_hostId = Lens.lens (\Placement' {hostId} -> hostId) (\s@Placement' {} a -> s {hostId = a} :: Placement)

-- | The number of the partition the instance is in. Valid only if the
-- placement group strategy is set to @partition@.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_partitionNumber :: Lens.Lens' Placement (Prelude.Maybe Prelude.Int)
placement_partitionNumber = Lens.lens (\Placement' {partitionNumber} -> partitionNumber) (\s@Placement' {} a -> s {partitionNumber = a} :: Placement)

-- | Reserved for future use.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_spreadDomain :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_spreadDomain = Lens.lens (\Placement' {spreadDomain} -> spreadDomain) (\s@Placement' {} a -> s {spreadDomain = a} :: Placement)

-- | The Availability Zone of the instance.
--
-- If not specified, an Availability Zone will be automatically chosen for
-- you based on the load balancing criteria for the Region.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_availabilityZone :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_availabilityZone = Lens.lens (\Placement' {availabilityZone} -> availabilityZone) (\s@Placement' {} a -> s {availabilityZone = a} :: Placement)

-- | The tenancy of the instance (if the instance is running in a VPC). An
-- instance with a tenancy of @dedicated@ runs on single-tenant hardware.
-- The @host@ tenancy is not supported for the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportInstance.html ImportInstance>
-- command.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
--
-- T3 instances that use the @unlimited@ CPU credit option do not support
-- @host@ tenancy.
placement_tenancy :: Lens.Lens' Placement (Prelude.Maybe Tenancy)
placement_tenancy = Lens.lens (\Placement' {tenancy} -> tenancy) (\s@Placement' {} a -> s {tenancy = a} :: Placement)

-- | The name of the placement group the instance is in.
placement_groupName :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_groupName = Lens.lens (\Placement' {groupName} -> groupName) (\s@Placement' {} a -> s {groupName = a} :: Placement)

-- | The ARN of the host resource group in which to launch the instances. If
-- you specify a host resource group ARN, omit the __Tenancy__ parameter or
-- set it to @host@.
--
-- This parameter is not supported by
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateFleet CreateFleet>.
placement_hostResourceGroupArn :: Lens.Lens' Placement (Prelude.Maybe Prelude.Text)
placement_hostResourceGroupArn = Lens.lens (\Placement' {hostResourceGroupArn} -> hostResourceGroupArn) (\s@Placement' {} a -> s {hostResourceGroupArn = a} :: Placement)

instance Core.FromXML Placement where
  parseXML x =
    Placement'
      Prelude.<$> (x Core..@? "affinity")
      Prelude.<*> (x Core..@? "hostId")
      Prelude.<*> (x Core..@? "partitionNumber")
      Prelude.<*> (x Core..@? "spreadDomain")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "tenancy")
      Prelude.<*> (x Core..@? "groupName")
      Prelude.<*> (x Core..@? "hostResourceGroupArn")

instance Prelude.Hashable Placement where
  hashWithSalt salt' Placement' {..} =
    salt' `Prelude.hashWithSalt` hostResourceGroupArn
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` tenancy
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` spreadDomain
      `Prelude.hashWithSalt` partitionNumber
      `Prelude.hashWithSalt` hostId
      `Prelude.hashWithSalt` affinity

instance Prelude.NFData Placement where
  rnf Placement' {..} =
    Prelude.rnf affinity
      `Prelude.seq` Prelude.rnf hostResourceGroupArn
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf tenancy
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf spreadDomain
      `Prelude.seq` Prelude.rnf partitionNumber
      `Prelude.seq` Prelude.rnf hostId

instance Core.ToQuery Placement where
  toQuery Placement' {..} =
    Prelude.mconcat
      [ "Affinity" Core.=: affinity,
        "HostId" Core.=: hostId,
        "PartitionNumber" Core.=: partitionNumber,
        "SpreadDomain" Core.=: spreadDomain,
        "AvailabilityZone" Core.=: availabilityZone,
        "Tenancy" Core.=: tenancy,
        "GroupName" Core.=: groupName,
        "HostResourceGroupArn" Core.=: hostResourceGroupArn
      ]
