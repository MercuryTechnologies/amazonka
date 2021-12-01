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
-- Module      : Amazonka.GameLift.Types.ScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.ScalingPolicy where

import qualified Amazonka.Core as Core
import Amazonka.GameLift.Types.ComparisonOperatorType
import Amazonka.GameLift.Types.LocationUpdateStatus
import Amazonka.GameLift.Types.MetricName
import Amazonka.GameLift.Types.PolicyType
import Amazonka.GameLift.Types.ScalingAdjustmentType
import Amazonka.GameLift.Types.ScalingStatusType
import Amazonka.GameLift.Types.TargetConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Rule that controls how a fleet is scaled. Scaling policies are uniquely
-- identified by the combination of name and fleet ID.
--
-- __Related actions__
--
-- DescribeFleetCapacity | UpdateFleetCapacity | DescribeEC2InstanceLimits
-- | PutScalingPolicy | DescribeScalingPolicies | DeleteScalingPolicy |
-- StopFleetActions | StartFleetActions |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newScalingPolicy' smart constructor.
data ScalingPolicy = ScalingPolicy'
  { -- | Current status of the scaling policy. The scaling policy can be in force
    -- only when in an @ACTIVE@ status. Scaling policies can be suspended for
    -- individual fleets (see StopFleetActions; if suspended for a fleet, the
    -- policy status does not change. View a fleet\'s stopped actions by
    -- calling DescribeFleetCapacity.
    --
    -- -   __ACTIVE__ -- The scaling policy can be used for auto-scaling a
    --     fleet.
    --
    -- -   __UPDATE_REQUESTED__ -- A request to update the scaling policy has
    --     been received.
    --
    -- -   __UPDATING__ -- A change is being made to the scaling policy.
    --
    -- -   __DELETE_REQUESTED__ -- A request to delete the scaling policy has
    --     been received.
    --
    -- -   __DELETING__ -- The scaling policy is being deleted.
    --
    -- -   __DELETED__ -- The scaling policy has been deleted.
    --
    -- -   __ERROR__ -- An error occurred in creating the policy. It should be
    --     removed and recreated.
    status :: Prelude.Maybe ScalingStatusType,
    -- | The type of adjustment to make to a fleet\'s instance count (see
    -- FleetCapacity):
    --
    -- -   __ChangeInCapacity__ -- add (or subtract) the scaling adjustment
    --     value from the current instance count. Positive values scale up
    --     while negative values scale down.
    --
    -- -   __ExactCapacity__ -- set the instance count to the scaling
    --     adjustment value.
    --
    -- -   __PercentChangeInCapacity__ -- increase or reduce the current
    --     instance count by the scaling adjustment, read as a percentage.
    --     Positive values scale up while negative values scale down.
    scalingAdjustmentType :: Prelude.Maybe ScalingAdjustmentType,
    location :: Prelude.Maybe Prelude.Text,
    -- | Length of time (in minutes) the metric must be at or beyond the
    -- threshold before a scaling event is triggered.
    evaluationPeriods :: Prelude.Maybe Prelude.Natural,
    -- | The type of scaling policy to create. For a target-based policy, set the
    -- parameter /MetricName/ to \'PercentAvailableGameSessions\' and specify a
    -- /TargetConfiguration/. For a rule-based policy set the following
    -- parameters: /MetricName/, /ComparisonOperator/, /Threshold/,
    -- /EvaluationPeriods/, /ScalingAdjustmentType/, and /ScalingAdjustment/.
    policyType :: Prelude.Maybe PolicyType,
    -- | Name of the Amazon GameLift-defined metric that is used to trigger a
    -- scaling adjustment. For detailed descriptions of fleet metrics, see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch>.
    --
    -- -   __ActivatingGameSessions__ -- Game sessions in the process of being
    --     created.
    --
    -- -   __ActiveGameSessions__ -- Game sessions that are currently running.
    --
    -- -   __ActiveInstances__ -- Fleet instances that are currently running at
    --     least one game session.
    --
    -- -   __AvailableGameSessions__ -- Additional game sessions that fleet
    --     could host simultaneously, given current capacity.
    --
    -- -   __AvailablePlayerSessions__ -- Empty player slots in currently
    --     active game sessions. This includes game sessions that are not
    --     currently accepting players. Reserved player slots are not included.
    --
    -- -   __CurrentPlayerSessions__ -- Player slots in active game sessions
    --     that are being used by a player or are reserved for a player.
    --
    -- -   __IdleInstances__ -- Active instances that are currently hosting
    --     zero game sessions.
    --
    -- -   __PercentAvailableGameSessions__ -- Unused percentage of the total
    --     number of game sessions that a fleet could host simultaneously,
    --     given current capacity. Use this metric for a target-based scaling
    --     policy.
    --
    -- -   __PercentIdleInstances__ -- Percentage of the total number of active
    --     instances that are hosting zero game sessions.
    --
    -- -   __QueueDepth__ -- Pending game session placement requests, in any
    --     queue, where the current fleet is the top-priority destination.
    --
    -- -   __WaitTime__ -- Current wait time for pending game session placement
    --     requests, in any queue, where the current fleet is the top-priority
    --     destination.
    metricName :: Prelude.Maybe MetricName,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- that is assigned to a GameLift fleet resource and uniquely identifies
    -- it. ARNs are unique across all Regions. Format is
    -- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | Comparison operator to use when measuring a metric against the threshold
    -- value.
    comparisonOperator :: Prelude.Maybe ComparisonOperatorType,
    -- | A descriptive label that is associated with a fleet\'s scaling policy.
    -- Policy names do not need to be unique.
    name :: Prelude.Maybe Prelude.Text,
    -- | Metric value used to trigger a scaling event.
    threshold :: Prelude.Maybe Prelude.Double,
    -- | Amount of adjustment to make, based on the scaling adjustment type.
    scalingAdjustment :: Prelude.Maybe Prelude.Int,
    -- | The current status of the fleet\'s scaling policies in a requested fleet
    -- location. The status @PENDING_UPDATE@ indicates that an update was
    -- requested for the fleet but has not yet been completed for the location.
    updateStatus :: Prelude.Maybe LocationUpdateStatus,
    -- | A unique identifier for the fleet that is associated with this scaling
    -- policy.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | An object that contains settings for a target-based scaling policy.
    targetConfiguration :: Prelude.Maybe TargetConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'scalingPolicy_status' - Current status of the scaling policy. The scaling policy can be in force
-- only when in an @ACTIVE@ status. Scaling policies can be suspended for
-- individual fleets (see StopFleetActions; if suspended for a fleet, the
-- policy status does not change. View a fleet\'s stopped actions by
-- calling DescribeFleetCapacity.
--
-- -   __ACTIVE__ -- The scaling policy can be used for auto-scaling a
--     fleet.
--
-- -   __UPDATE_REQUESTED__ -- A request to update the scaling policy has
--     been received.
--
-- -   __UPDATING__ -- A change is being made to the scaling policy.
--
-- -   __DELETE_REQUESTED__ -- A request to delete the scaling policy has
--     been received.
--
-- -   __DELETING__ -- The scaling policy is being deleted.
--
-- -   __DELETED__ -- The scaling policy has been deleted.
--
-- -   __ERROR__ -- An error occurred in creating the policy. It should be
--     removed and recreated.
--
-- 'scalingAdjustmentType', 'scalingPolicy_scalingAdjustmentType' - The type of adjustment to make to a fleet\'s instance count (see
-- FleetCapacity):
--
-- -   __ChangeInCapacity__ -- add (or subtract) the scaling adjustment
--     value from the current instance count. Positive values scale up
--     while negative values scale down.
--
-- -   __ExactCapacity__ -- set the instance count to the scaling
--     adjustment value.
--
-- -   __PercentChangeInCapacity__ -- increase or reduce the current
--     instance count by the scaling adjustment, read as a percentage.
--     Positive values scale up while negative values scale down.
--
-- 'location', 'scalingPolicy_location' -
--
-- 'evaluationPeriods', 'scalingPolicy_evaluationPeriods' - Length of time (in minutes) the metric must be at or beyond the
-- threshold before a scaling event is triggered.
--
-- 'policyType', 'scalingPolicy_policyType' - The type of scaling policy to create. For a target-based policy, set the
-- parameter /MetricName/ to \'PercentAvailableGameSessions\' and specify a
-- /TargetConfiguration/. For a rule-based policy set the following
-- parameters: /MetricName/, /ComparisonOperator/, /Threshold/,
-- /EvaluationPeriods/, /ScalingAdjustmentType/, and /ScalingAdjustment/.
--
-- 'metricName', 'scalingPolicy_metricName' - Name of the Amazon GameLift-defined metric that is used to trigger a
-- scaling adjustment. For detailed descriptions of fleet metrics, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch>.
--
-- -   __ActivatingGameSessions__ -- Game sessions in the process of being
--     created.
--
-- -   __ActiveGameSessions__ -- Game sessions that are currently running.
--
-- -   __ActiveInstances__ -- Fleet instances that are currently running at
--     least one game session.
--
-- -   __AvailableGameSessions__ -- Additional game sessions that fleet
--     could host simultaneously, given current capacity.
--
-- -   __AvailablePlayerSessions__ -- Empty player slots in currently
--     active game sessions. This includes game sessions that are not
--     currently accepting players. Reserved player slots are not included.
--
-- -   __CurrentPlayerSessions__ -- Player slots in active game sessions
--     that are being used by a player or are reserved for a player.
--
-- -   __IdleInstances__ -- Active instances that are currently hosting
--     zero game sessions.
--
-- -   __PercentAvailableGameSessions__ -- Unused percentage of the total
--     number of game sessions that a fleet could host simultaneously,
--     given current capacity. Use this metric for a target-based scaling
--     policy.
--
-- -   __PercentIdleInstances__ -- Percentage of the total number of active
--     instances that are hosting zero game sessions.
--
-- -   __QueueDepth__ -- Pending game session placement requests, in any
--     queue, where the current fleet is the top-priority destination.
--
-- -   __WaitTime__ -- Current wait time for pending game session placement
--     requests, in any queue, where the current fleet is the top-priority
--     destination.
--
-- 'fleetArn', 'scalingPolicy_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
--
-- 'comparisonOperator', 'scalingPolicy_comparisonOperator' - Comparison operator to use when measuring a metric against the threshold
-- value.
--
-- 'name', 'scalingPolicy_name' - A descriptive label that is associated with a fleet\'s scaling policy.
-- Policy names do not need to be unique.
--
-- 'threshold', 'scalingPolicy_threshold' - Metric value used to trigger a scaling event.
--
-- 'scalingAdjustment', 'scalingPolicy_scalingAdjustment' - Amount of adjustment to make, based on the scaling adjustment type.
--
-- 'updateStatus', 'scalingPolicy_updateStatus' - The current status of the fleet\'s scaling policies in a requested fleet
-- location. The status @PENDING_UPDATE@ indicates that an update was
-- requested for the fleet but has not yet been completed for the location.
--
-- 'fleetId', 'scalingPolicy_fleetId' - A unique identifier for the fleet that is associated with this scaling
-- policy.
--
-- 'targetConfiguration', 'scalingPolicy_targetConfiguration' - An object that contains settings for a target-based scaling policy.
newScalingPolicy ::
  ScalingPolicy
newScalingPolicy =
  ScalingPolicy'
    { status = Prelude.Nothing,
      scalingAdjustmentType = Prelude.Nothing,
      location = Prelude.Nothing,
      evaluationPeriods = Prelude.Nothing,
      policyType = Prelude.Nothing,
      metricName = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      comparisonOperator = Prelude.Nothing,
      name = Prelude.Nothing,
      threshold = Prelude.Nothing,
      scalingAdjustment = Prelude.Nothing,
      updateStatus = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      targetConfiguration = Prelude.Nothing
    }

-- | Current status of the scaling policy. The scaling policy can be in force
-- only when in an @ACTIVE@ status. Scaling policies can be suspended for
-- individual fleets (see StopFleetActions; if suspended for a fleet, the
-- policy status does not change. View a fleet\'s stopped actions by
-- calling DescribeFleetCapacity.
--
-- -   __ACTIVE__ -- The scaling policy can be used for auto-scaling a
--     fleet.
--
-- -   __UPDATE_REQUESTED__ -- A request to update the scaling policy has
--     been received.
--
-- -   __UPDATING__ -- A change is being made to the scaling policy.
--
-- -   __DELETE_REQUESTED__ -- A request to delete the scaling policy has
--     been received.
--
-- -   __DELETING__ -- The scaling policy is being deleted.
--
-- -   __DELETED__ -- The scaling policy has been deleted.
--
-- -   __ERROR__ -- An error occurred in creating the policy. It should be
--     removed and recreated.
scalingPolicy_status :: Lens.Lens' ScalingPolicy (Prelude.Maybe ScalingStatusType)
scalingPolicy_status = Lens.lens (\ScalingPolicy' {status} -> status) (\s@ScalingPolicy' {} a -> s {status = a} :: ScalingPolicy)

-- | The type of adjustment to make to a fleet\'s instance count (see
-- FleetCapacity):
--
-- -   __ChangeInCapacity__ -- add (or subtract) the scaling adjustment
--     value from the current instance count. Positive values scale up
--     while negative values scale down.
--
-- -   __ExactCapacity__ -- set the instance count to the scaling
--     adjustment value.
--
-- -   __PercentChangeInCapacity__ -- increase or reduce the current
--     instance count by the scaling adjustment, read as a percentage.
--     Positive values scale up while negative values scale down.
scalingPolicy_scalingAdjustmentType :: Lens.Lens' ScalingPolicy (Prelude.Maybe ScalingAdjustmentType)
scalingPolicy_scalingAdjustmentType = Lens.lens (\ScalingPolicy' {scalingAdjustmentType} -> scalingAdjustmentType) (\s@ScalingPolicy' {} a -> s {scalingAdjustmentType = a} :: ScalingPolicy)

-- |
scalingPolicy_location :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_location = Lens.lens (\ScalingPolicy' {location} -> location) (\s@ScalingPolicy' {} a -> s {location = a} :: ScalingPolicy)

-- | Length of time (in minutes) the metric must be at or beyond the
-- threshold before a scaling event is triggered.
scalingPolicy_evaluationPeriods :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Natural)
scalingPolicy_evaluationPeriods = Lens.lens (\ScalingPolicy' {evaluationPeriods} -> evaluationPeriods) (\s@ScalingPolicy' {} a -> s {evaluationPeriods = a} :: ScalingPolicy)

-- | The type of scaling policy to create. For a target-based policy, set the
-- parameter /MetricName/ to \'PercentAvailableGameSessions\' and specify a
-- /TargetConfiguration/. For a rule-based policy set the following
-- parameters: /MetricName/, /ComparisonOperator/, /Threshold/,
-- /EvaluationPeriods/, /ScalingAdjustmentType/, and /ScalingAdjustment/.
scalingPolicy_policyType :: Lens.Lens' ScalingPolicy (Prelude.Maybe PolicyType)
scalingPolicy_policyType = Lens.lens (\ScalingPolicy' {policyType} -> policyType) (\s@ScalingPolicy' {} a -> s {policyType = a} :: ScalingPolicy)

-- | Name of the Amazon GameLift-defined metric that is used to trigger a
-- scaling adjustment. For detailed descriptions of fleet metrics, see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/monitoring-cloudwatch.html Monitor Amazon GameLift with Amazon CloudWatch>.
--
-- -   __ActivatingGameSessions__ -- Game sessions in the process of being
--     created.
--
-- -   __ActiveGameSessions__ -- Game sessions that are currently running.
--
-- -   __ActiveInstances__ -- Fleet instances that are currently running at
--     least one game session.
--
-- -   __AvailableGameSessions__ -- Additional game sessions that fleet
--     could host simultaneously, given current capacity.
--
-- -   __AvailablePlayerSessions__ -- Empty player slots in currently
--     active game sessions. This includes game sessions that are not
--     currently accepting players. Reserved player slots are not included.
--
-- -   __CurrentPlayerSessions__ -- Player slots in active game sessions
--     that are being used by a player or are reserved for a player.
--
-- -   __IdleInstances__ -- Active instances that are currently hosting
--     zero game sessions.
--
-- -   __PercentAvailableGameSessions__ -- Unused percentage of the total
--     number of game sessions that a fleet could host simultaneously,
--     given current capacity. Use this metric for a target-based scaling
--     policy.
--
-- -   __PercentIdleInstances__ -- Percentage of the total number of active
--     instances that are hosting zero game sessions.
--
-- -   __QueueDepth__ -- Pending game session placement requests, in any
--     queue, where the current fleet is the top-priority destination.
--
-- -   __WaitTime__ -- Current wait time for pending game session placement
--     requests, in any queue, where the current fleet is the top-priority
--     destination.
scalingPolicy_metricName :: Lens.Lens' ScalingPolicy (Prelude.Maybe MetricName)
scalingPolicy_metricName = Lens.lens (\ScalingPolicy' {metricName} -> metricName) (\s@ScalingPolicy' {} a -> s {metricName = a} :: ScalingPolicy)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- that is assigned to a GameLift fleet resource and uniquely identifies
-- it. ARNs are unique across all Regions. Format is
-- @arn:aws:gamelift:\<region>::fleet\/fleet-a1234567-b8c9-0d1e-2fa3-b45c6d7e8912@.
scalingPolicy_fleetArn :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_fleetArn = Lens.lens (\ScalingPolicy' {fleetArn} -> fleetArn) (\s@ScalingPolicy' {} a -> s {fleetArn = a} :: ScalingPolicy)

-- | Comparison operator to use when measuring a metric against the threshold
-- value.
scalingPolicy_comparisonOperator :: Lens.Lens' ScalingPolicy (Prelude.Maybe ComparisonOperatorType)
scalingPolicy_comparisonOperator = Lens.lens (\ScalingPolicy' {comparisonOperator} -> comparisonOperator) (\s@ScalingPolicy' {} a -> s {comparisonOperator = a} :: ScalingPolicy)

-- | A descriptive label that is associated with a fleet\'s scaling policy.
-- Policy names do not need to be unique.
scalingPolicy_name :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_name = Lens.lens (\ScalingPolicy' {name} -> name) (\s@ScalingPolicy' {} a -> s {name = a} :: ScalingPolicy)

-- | Metric value used to trigger a scaling event.
scalingPolicy_threshold :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Double)
scalingPolicy_threshold = Lens.lens (\ScalingPolicy' {threshold} -> threshold) (\s@ScalingPolicy' {} a -> s {threshold = a} :: ScalingPolicy)

-- | Amount of adjustment to make, based on the scaling adjustment type.
scalingPolicy_scalingAdjustment :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Int)
scalingPolicy_scalingAdjustment = Lens.lens (\ScalingPolicy' {scalingAdjustment} -> scalingAdjustment) (\s@ScalingPolicy' {} a -> s {scalingAdjustment = a} :: ScalingPolicy)

-- | The current status of the fleet\'s scaling policies in a requested fleet
-- location. The status @PENDING_UPDATE@ indicates that an update was
-- requested for the fleet but has not yet been completed for the location.
scalingPolicy_updateStatus :: Lens.Lens' ScalingPolicy (Prelude.Maybe LocationUpdateStatus)
scalingPolicy_updateStatus = Lens.lens (\ScalingPolicy' {updateStatus} -> updateStatus) (\s@ScalingPolicy' {} a -> s {updateStatus = a} :: ScalingPolicy)

-- | A unique identifier for the fleet that is associated with this scaling
-- policy.
scalingPolicy_fleetId :: Lens.Lens' ScalingPolicy (Prelude.Maybe Prelude.Text)
scalingPolicy_fleetId = Lens.lens (\ScalingPolicy' {fleetId} -> fleetId) (\s@ScalingPolicy' {} a -> s {fleetId = a} :: ScalingPolicy)

-- | An object that contains settings for a target-based scaling policy.
scalingPolicy_targetConfiguration :: Lens.Lens' ScalingPolicy (Prelude.Maybe TargetConfiguration)
scalingPolicy_targetConfiguration = Lens.lens (\ScalingPolicy' {targetConfiguration} -> targetConfiguration) (\s@ScalingPolicy' {} a -> s {targetConfiguration = a} :: ScalingPolicy)

instance Core.FromJSON ScalingPolicy where
  parseJSON =
    Core.withObject
      "ScalingPolicy"
      ( \x ->
          ScalingPolicy'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ScalingAdjustmentType")
            Prelude.<*> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "EvaluationPeriods")
            Prelude.<*> (x Core..:? "PolicyType")
            Prelude.<*> (x Core..:? "MetricName")
            Prelude.<*> (x Core..:? "FleetArn")
            Prelude.<*> (x Core..:? "ComparisonOperator")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Threshold")
            Prelude.<*> (x Core..:? "ScalingAdjustment")
            Prelude.<*> (x Core..:? "UpdateStatus")
            Prelude.<*> (x Core..:? "FleetId")
            Prelude.<*> (x Core..:? "TargetConfiguration")
      )

instance Prelude.Hashable ScalingPolicy where
  hashWithSalt salt' ScalingPolicy' {..} =
    salt' `Prelude.hashWithSalt` targetConfiguration
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` updateStatus
      `Prelude.hashWithSalt` scalingAdjustment
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` evaluationPeriods
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` scalingAdjustmentType
      `Prelude.hashWithSalt` status

instance Prelude.NFData ScalingPolicy where
  rnf ScalingPolicy' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetConfiguration
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf updateStatus
      `Prelude.seq` Prelude.rnf scalingAdjustment
      `Prelude.seq` Prelude.rnf threshold
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf comparisonOperator
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf evaluationPeriods
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf scalingAdjustmentType
