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
-- Module      : Amazonka.GameLift.Types.FleetCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.FleetCapacity where

import qualified Amazonka.Core as Core
import Amazonka.GameLift.Types.EC2InstanceCounts
import Amazonka.GameLift.Types.EC2InstanceType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Current resource capacity settings in a specified fleet or location. The
-- location value might refer to a fleet\'s remote location or its home
-- Region.
--
-- __Related actions__
--
-- DescribeFleetCapacity | DescribeFleetLocationCapacity |
-- UpdateFleetCapacity
--
-- /See:/ 'newFleetCapacity' smart constructor.
data FleetCapacity = FleetCapacity'
  { -- | The fleet location for the instance count information, expressed as an
    -- AWS Region code, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | The EC2 instance type that is used for all instances in a fleet. The
    -- instance type determines the computing resources in use, including CPU,
    -- memory, storage, and networking capacity. See
    -- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
    -- for detailed descriptions.
    instanceType :: Prelude.Maybe EC2InstanceType,
    -- | A unique identifier for the fleet associated with the location.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | The current instance count and capacity settings for the fleet location.
    instanceCounts :: Prelude.Maybe EC2InstanceCounts
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'fleetCapacity_location' - The fleet location for the instance count information, expressed as an
-- AWS Region code, such as @us-west-2@.
--
-- 'fleetArn', 'fleetCapacity_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'instanceType', 'fleetCapacity_instanceType' - The EC2 instance type that is used for all instances in a fleet. The
-- instance type determines the computing resources in use, including CPU,
-- memory, storage, and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
--
-- 'fleetId', 'fleetCapacity_fleetId' - A unique identifier for the fleet associated with the location.
--
-- 'instanceCounts', 'fleetCapacity_instanceCounts' - The current instance count and capacity settings for the fleet location.
newFleetCapacity ::
  FleetCapacity
newFleetCapacity =
  FleetCapacity'
    { location = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      instanceCounts = Prelude.Nothing
    }

-- | The fleet location for the instance count information, expressed as an
-- AWS Region code, such as @us-west-2@.
fleetCapacity_location :: Lens.Lens' FleetCapacity (Prelude.Maybe Prelude.Text)
fleetCapacity_location = Lens.lens (\FleetCapacity' {location} -> location) (\s@FleetCapacity' {} a -> s {location = a} :: FleetCapacity)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
fleetCapacity_fleetArn :: Lens.Lens' FleetCapacity (Prelude.Maybe Prelude.Text)
fleetCapacity_fleetArn = Lens.lens (\FleetCapacity' {fleetArn} -> fleetArn) (\s@FleetCapacity' {} a -> s {fleetArn = a} :: FleetCapacity)

-- | The EC2 instance type that is used for all instances in a fleet. The
-- instance type determines the computing resources in use, including CPU,
-- memory, storage, and networking capacity. See
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>
-- for detailed descriptions.
fleetCapacity_instanceType :: Lens.Lens' FleetCapacity (Prelude.Maybe EC2InstanceType)
fleetCapacity_instanceType = Lens.lens (\FleetCapacity' {instanceType} -> instanceType) (\s@FleetCapacity' {} a -> s {instanceType = a} :: FleetCapacity)

-- | A unique identifier for the fleet associated with the location.
fleetCapacity_fleetId :: Lens.Lens' FleetCapacity (Prelude.Maybe Prelude.Text)
fleetCapacity_fleetId = Lens.lens (\FleetCapacity' {fleetId} -> fleetId) (\s@FleetCapacity' {} a -> s {fleetId = a} :: FleetCapacity)

-- | The current instance count and capacity settings for the fleet location.
fleetCapacity_instanceCounts :: Lens.Lens' FleetCapacity (Prelude.Maybe EC2InstanceCounts)
fleetCapacity_instanceCounts = Lens.lens (\FleetCapacity' {instanceCounts} -> instanceCounts) (\s@FleetCapacity' {} a -> s {instanceCounts = a} :: FleetCapacity)

instance Core.FromJSON FleetCapacity where
  parseJSON =
    Core.withObject
      "FleetCapacity"
      ( \x ->
          FleetCapacity'
            Prelude.<$> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "FleetArn")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "FleetId")
            Prelude.<*> (x Core..:? "InstanceCounts")
      )

instance Prelude.Hashable FleetCapacity where
  hashWithSalt salt' FleetCapacity' {..} =
    salt' `Prelude.hashWithSalt` instanceCounts
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` location

instance Prelude.NFData FleetCapacity where
  rnf FleetCapacity' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf instanceCounts
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf fleetArn
