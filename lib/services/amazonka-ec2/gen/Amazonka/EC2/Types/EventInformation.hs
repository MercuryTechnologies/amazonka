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
-- Module      : Amazonka.EC2.Types.EventInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EventInformation where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an EC2 Fleet or Spot Fleet event.
--
-- /See:/ 'newEventInformation' smart constructor.
data EventInformation = EventInformation'
  { -- | The ID of the instance. This information is available only for
    -- @instanceChange@ events.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The description of the event.
    eventDescription :: Prelude.Maybe Prelude.Text,
    -- | The event.
    --
    -- @error@ events:
    --
    -- -   @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet does not have
    --     the required permissions either to launch or terminate an instance.
    --
    -- -   @allLaunchSpecsTemporarilyBlacklisted@ - None of the configurations
    --     are valid, and several attempts to launch instances have failed. For
    --     more information, see the description of the event.
    --
    -- -   @spotInstanceCountLimitExceeded@ - You\'ve reached the limit on the
    --     number of Spot Instances that you can launch.
    --
    -- -   @spotFleetRequestConfigurationInvalid@ - The configuration is not
    --     valid. For more information, see the description of the event.
    --
    -- @fleetRequestChange@ events:
    --
    -- -   @active@ - The EC2 Fleet or Spot Fleet request has been validated
    --     and Amazon EC2 is attempting to maintain the target number of
    --     running instances.
    --
    -- -   @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and
    --     has no running instances. The EC2 Fleet or Spot Fleet will be
    --     deleted two days after its instances are terminated.
    --
    -- -   @cancelled_running@ - The EC2 Fleet or Spot Fleet request is
    --     canceled and does not launch additional instances. Its existing
    --     instances continue to run until they are interrupted or terminated.
    --     The request remains in this state until all instances are
    --     interrupted or terminated.
    --
    -- -   @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is
    --     canceled and its instances are terminating. The request remains in
    --     this state until all instances are terminated.
    --
    -- -   @expired@ - The EC2 Fleet or Spot Fleet request has expired. If the
    --     request was created with @TerminateInstancesWithExpiration@ set, a
    --     subsequent @terminated@ event indicates that the instances are
    --     terminated.
    --
    -- -   @modify_in_progress@ - The EC2 Fleet or Spot Fleet request is being
    --     modified. The request remains in this state until the modification
    --     is fully processed.
    --
    -- -   @modify_succeeded@ - The EC2 Fleet or Spot Fleet request was
    --     modified.
    --
    -- -   @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated
    --     and Amazon EC2 is preparing to launch the target number of
    --     instances.
    --
    -- -   @progress@ - The EC2 Fleet or Spot Fleet request is in the process
    --     of being fulfilled.
    --
    -- @instanceChange@ events:
    --
    -- -   @launched@ - A new instance was launched.
    --
    -- -   @terminated@ - An instance was terminated by the user.
    --
    -- -   @termination_notified@ - An instance termination notification was
    --     sent when a Spot Instance was terminated by Amazon EC2 during
    --     scale-down, when the target capacity of the fleet was modified down,
    --     for example, from a target capacity of 4 to a target capacity of 3.
    --
    -- @Information@ events:
    --
    -- -   @fleetProgressHalted@ - The price in every launch specification is
    --     not valid because it is below the Spot price (all the launch
    --     specifications have produced @launchSpecUnusable@ events). A launch
    --     specification might become valid if the Spot price changes.
    --
    -- -   @launchSpecTemporarilyBlacklisted@ - The configuration is not valid
    --     and several attempts to launch instances have failed. For more
    --     information, see the description of the event.
    --
    -- -   @launchSpecUnusable@ - The price in a launch specification is not
    --     valid because it is below the Spot price.
    --
    -- -   @registerWithLoadBalancersFailed@ - An attempt to register instances
    --     with load balancers failed. For more information, see the
    --     description of the event.
    eventSubType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'eventInformation_instanceId' - The ID of the instance. This information is available only for
-- @instanceChange@ events.
--
-- 'eventDescription', 'eventInformation_eventDescription' - The description of the event.
--
-- 'eventSubType', 'eventInformation_eventSubType' - The event.
--
-- @error@ events:
--
-- -   @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet does not have
--     the required permissions either to launch or terminate an instance.
--
-- -   @allLaunchSpecsTemporarilyBlacklisted@ - None of the configurations
--     are valid, and several attempts to launch instances have failed. For
--     more information, see the description of the event.
--
-- -   @spotInstanceCountLimitExceeded@ - You\'ve reached the limit on the
--     number of Spot Instances that you can launch.
--
-- -   @spotFleetRequestConfigurationInvalid@ - The configuration is not
--     valid. For more information, see the description of the event.
--
-- @fleetRequestChange@ events:
--
-- -   @active@ - The EC2 Fleet or Spot Fleet request has been validated
--     and Amazon EC2 is attempting to maintain the target number of
--     running instances.
--
-- -   @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and
--     has no running instances. The EC2 Fleet or Spot Fleet will be
--     deleted two days after its instances are terminated.
--
-- -   @cancelled_running@ - The EC2 Fleet or Spot Fleet request is
--     canceled and does not launch additional instances. Its existing
--     instances continue to run until they are interrupted or terminated.
--     The request remains in this state until all instances are
--     interrupted or terminated.
--
-- -   @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is
--     canceled and its instances are terminating. The request remains in
--     this state until all instances are terminated.
--
-- -   @expired@ - The EC2 Fleet or Spot Fleet request has expired. If the
--     request was created with @TerminateInstancesWithExpiration@ set, a
--     subsequent @terminated@ event indicates that the instances are
--     terminated.
--
-- -   @modify_in_progress@ - The EC2 Fleet or Spot Fleet request is being
--     modified. The request remains in this state until the modification
--     is fully processed.
--
-- -   @modify_succeeded@ - The EC2 Fleet or Spot Fleet request was
--     modified.
--
-- -   @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated
--     and Amazon EC2 is preparing to launch the target number of
--     instances.
--
-- -   @progress@ - The EC2 Fleet or Spot Fleet request is in the process
--     of being fulfilled.
--
-- @instanceChange@ events:
--
-- -   @launched@ - A new instance was launched.
--
-- -   @terminated@ - An instance was terminated by the user.
--
-- -   @termination_notified@ - An instance termination notification was
--     sent when a Spot Instance was terminated by Amazon EC2 during
--     scale-down, when the target capacity of the fleet was modified down,
--     for example, from a target capacity of 4 to a target capacity of 3.
--
-- @Information@ events:
--
-- -   @fleetProgressHalted@ - The price in every launch specification is
--     not valid because it is below the Spot price (all the launch
--     specifications have produced @launchSpecUnusable@ events). A launch
--     specification might become valid if the Spot price changes.
--
-- -   @launchSpecTemporarilyBlacklisted@ - The configuration is not valid
--     and several attempts to launch instances have failed. For more
--     information, see the description of the event.
--
-- -   @launchSpecUnusable@ - The price in a launch specification is not
--     valid because it is below the Spot price.
--
-- -   @registerWithLoadBalancersFailed@ - An attempt to register instances
--     with load balancers failed. For more information, see the
--     description of the event.
newEventInformation ::
  EventInformation
newEventInformation =
  EventInformation'
    { instanceId = Prelude.Nothing,
      eventDescription = Prelude.Nothing,
      eventSubType = Prelude.Nothing
    }

-- | The ID of the instance. This information is available only for
-- @instanceChange@ events.
eventInformation_instanceId :: Lens.Lens' EventInformation (Prelude.Maybe Prelude.Text)
eventInformation_instanceId = Lens.lens (\EventInformation' {instanceId} -> instanceId) (\s@EventInformation' {} a -> s {instanceId = a} :: EventInformation)

-- | The description of the event.
eventInformation_eventDescription :: Lens.Lens' EventInformation (Prelude.Maybe Prelude.Text)
eventInformation_eventDescription = Lens.lens (\EventInformation' {eventDescription} -> eventDescription) (\s@EventInformation' {} a -> s {eventDescription = a} :: EventInformation)

-- | The event.
--
-- @error@ events:
--
-- -   @iamFleetRoleInvalid@ - The EC2 Fleet or Spot Fleet does not have
--     the required permissions either to launch or terminate an instance.
--
-- -   @allLaunchSpecsTemporarilyBlacklisted@ - None of the configurations
--     are valid, and several attempts to launch instances have failed. For
--     more information, see the description of the event.
--
-- -   @spotInstanceCountLimitExceeded@ - You\'ve reached the limit on the
--     number of Spot Instances that you can launch.
--
-- -   @spotFleetRequestConfigurationInvalid@ - The configuration is not
--     valid. For more information, see the description of the event.
--
-- @fleetRequestChange@ events:
--
-- -   @active@ - The EC2 Fleet or Spot Fleet request has been validated
--     and Amazon EC2 is attempting to maintain the target number of
--     running instances.
--
-- -   @cancelled@ - The EC2 Fleet or Spot Fleet request is canceled and
--     has no running instances. The EC2 Fleet or Spot Fleet will be
--     deleted two days after its instances are terminated.
--
-- -   @cancelled_running@ - The EC2 Fleet or Spot Fleet request is
--     canceled and does not launch additional instances. Its existing
--     instances continue to run until they are interrupted or terminated.
--     The request remains in this state until all instances are
--     interrupted or terminated.
--
-- -   @cancelled_terminating@ - The EC2 Fleet or Spot Fleet request is
--     canceled and its instances are terminating. The request remains in
--     this state until all instances are terminated.
--
-- -   @expired@ - The EC2 Fleet or Spot Fleet request has expired. If the
--     request was created with @TerminateInstancesWithExpiration@ set, a
--     subsequent @terminated@ event indicates that the instances are
--     terminated.
--
-- -   @modify_in_progress@ - The EC2 Fleet or Spot Fleet request is being
--     modified. The request remains in this state until the modification
--     is fully processed.
--
-- -   @modify_succeeded@ - The EC2 Fleet or Spot Fleet request was
--     modified.
--
-- -   @submitted@ - The EC2 Fleet or Spot Fleet request is being evaluated
--     and Amazon EC2 is preparing to launch the target number of
--     instances.
--
-- -   @progress@ - The EC2 Fleet or Spot Fleet request is in the process
--     of being fulfilled.
--
-- @instanceChange@ events:
--
-- -   @launched@ - A new instance was launched.
--
-- -   @terminated@ - An instance was terminated by the user.
--
-- -   @termination_notified@ - An instance termination notification was
--     sent when a Spot Instance was terminated by Amazon EC2 during
--     scale-down, when the target capacity of the fleet was modified down,
--     for example, from a target capacity of 4 to a target capacity of 3.
--
-- @Information@ events:
--
-- -   @fleetProgressHalted@ - The price in every launch specification is
--     not valid because it is below the Spot price (all the launch
--     specifications have produced @launchSpecUnusable@ events). A launch
--     specification might become valid if the Spot price changes.
--
-- -   @launchSpecTemporarilyBlacklisted@ - The configuration is not valid
--     and several attempts to launch instances have failed. For more
--     information, see the description of the event.
--
-- -   @launchSpecUnusable@ - The price in a launch specification is not
--     valid because it is below the Spot price.
--
-- -   @registerWithLoadBalancersFailed@ - An attempt to register instances
--     with load balancers failed. For more information, see the
--     description of the event.
eventInformation_eventSubType :: Lens.Lens' EventInformation (Prelude.Maybe Prelude.Text)
eventInformation_eventSubType = Lens.lens (\EventInformation' {eventSubType} -> eventSubType) (\s@EventInformation' {} a -> s {eventSubType = a} :: EventInformation)

instance Core.FromXML EventInformation where
  parseXML x =
    EventInformation'
      Prelude.<$> (x Core..@? "instanceId")
      Prelude.<*> (x Core..@? "eventDescription")
      Prelude.<*> (x Core..@? "eventSubType")

instance Prelude.Hashable EventInformation where
  hashWithSalt salt' EventInformation' {..} =
    salt' `Prelude.hashWithSalt` eventSubType
      `Prelude.hashWithSalt` eventDescription
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData EventInformation where
  rnf EventInformation' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf eventSubType
      `Prelude.seq` Prelude.rnf eventDescription
