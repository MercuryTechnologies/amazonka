{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AutoScaling.PutScalingPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a scaling policy for an Auto Scaling group. Scaling
-- policies are used to scale an Auto Scaling group based on configurable
-- metrics. If no policies are defined, the dynamic scaling and predictive
-- scaling features are not used.
--
-- For more information about using dynamic scaling, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-target-tracking.html Target tracking scaling policies>
-- and
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html Step and simple scaling policies>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- For more information about using predictive scaling, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ec2-auto-scaling-predictive-scaling.html Predictive scaling for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- You can view the scaling policies for an Auto Scaling group using the
-- DescribePolicies API call. If you are no longer using a scaling policy,
-- you can delete it by calling the DeletePolicy API.
module Amazonka.AutoScaling.PutScalingPolicy
  ( -- * Creating a Request
    PutScalingPolicy (..),
    newPutScalingPolicy,

    -- * Request Lenses
    putScalingPolicy_minAdjustmentStep,
    putScalingPolicy_estimatedInstanceWarmup,
    putScalingPolicy_enabled,
    putScalingPolicy_policyType,
    putScalingPolicy_stepAdjustments,
    putScalingPolicy_targetTrackingConfiguration,
    putScalingPolicy_adjustmentType,
    putScalingPolicy_predictiveScalingConfiguration,
    putScalingPolicy_scalingAdjustment,
    putScalingPolicy_cooldown,
    putScalingPolicy_metricAggregationType,
    putScalingPolicy_minAdjustmentMagnitude,
    putScalingPolicy_autoScalingGroupName,
    putScalingPolicy_policyName,

    -- * Destructuring the Response
    PutScalingPolicyResponse (..),
    newPutScalingPolicyResponse,

    -- * Response Lenses
    putScalingPolicyResponse_policyARN,
    putScalingPolicyResponse_alarms,
    putScalingPolicyResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutScalingPolicy' smart constructor.
data PutScalingPolicy = PutScalingPolicy'
  { -- | Available for backward compatibility. Use @MinAdjustmentMagnitude@
    -- instead.
    minAdjustmentStep :: Prelude.Maybe Prelude.Int,
    -- | The estimated time, in seconds, until a newly launched instance can
    -- contribute to the CloudWatch metrics. If not provided, the default is to
    -- use the value from the default cooldown period for the Auto Scaling
    -- group.
    --
    -- Valid only if the policy type is @TargetTrackingScaling@ or
    -- @StepScaling@.
    estimatedInstanceWarmup :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the scaling policy is enabled or disabled. The default
    -- is enabled. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | One of the following policy types:
    --
    -- -   @TargetTrackingScaling@
    --
    -- -   @StepScaling@
    --
    -- -   @SimpleScaling@ (default)
    --
    -- -   @PredictiveScaling@
    policyType :: Prelude.Maybe Prelude.Text,
    -- | A set of adjustments that enable you to scale based on the size of the
    -- alarm breach.
    --
    -- Required if the policy type is @StepScaling@. (Not used with any other
    -- policy type.)
    stepAdjustments :: Prelude.Maybe [StepAdjustment],
    -- | A target tracking scaling policy. Provides support for predefined or
    -- customized metrics.
    --
    -- The following predefined metrics are available:
    --
    -- -   @ASGAverageCPUUtilization@
    --
    -- -   @ASGAverageNetworkIn@
    --
    -- -   @ASGAverageNetworkOut@
    --
    -- -   @ALBRequestCountPerTarget@
    --
    -- If you specify @ALBRequestCountPerTarget@ for the metric, you must
    -- specify the @ResourceLabel@ parameter with the
    -- @PredefinedMetricSpecification@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_TargetTrackingConfiguration.html TargetTrackingConfiguration>
    -- in the /Amazon EC2 Auto Scaling API Reference/.
    --
    -- Required if the policy type is @TargetTrackingScaling@.
    targetTrackingConfiguration :: Prelude.Maybe TargetTrackingConfiguration,
    -- | Specifies how the scaling adjustment is interpreted (for example, an
    -- absolute number or a percentage). The valid values are
    -- @ChangeInCapacity@, @ExactCapacity@, and @PercentChangeInCapacity@.
    --
    -- Required if the policy type is @StepScaling@ or @SimpleScaling@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    adjustmentType :: Prelude.Maybe Prelude.Text,
    -- | A predictive scaling policy. Provides support for only predefined
    -- metrics.
    --
    -- Predictive scaling works with CPU utilization, network in\/out, and the
    -- Application Load Balancer request count.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_PredictiveScalingConfiguration.html PredictiveScalingConfiguration>
    -- in the /Amazon EC2 Auto Scaling API Reference/.
    --
    -- Required if the policy type is @PredictiveScaling@.
    predictiveScalingConfiguration :: Prelude.Maybe PredictiveScalingConfiguration,
    -- | The amount by which to scale, based on the specified adjustment type. A
    -- positive value adds to the current capacity while a negative number
    -- removes from the current capacity. For exact capacity, you must specify
    -- a positive value.
    --
    -- Required if the policy type is @SimpleScaling@. (Not used with any other
    -- policy type.)
    scalingAdjustment :: Prelude.Maybe Prelude.Int,
    -- | The duration of the policy\'s cooldown period, in seconds. When a
    -- cooldown period is specified here, it overrides the default cooldown
    -- period defined for the Auto Scaling group.
    --
    -- Valid only if the policy type is @SimpleScaling@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    cooldown :: Prelude.Maybe Prelude.Int,
    -- | The aggregation type for the CloudWatch metrics. The valid values are
    -- @Minimum@, @Maximum@, and @Average@. If the aggregation type is null,
    -- the value is treated as @Average@.
    --
    -- Valid only if the policy type is @StepScaling@.
    metricAggregationType :: Prelude.Maybe Prelude.Text,
    -- | The minimum value to scale by when the adjustment type is
    -- @PercentChangeInCapacity@. For example, suppose that you create a step
    -- scaling policy to scale out an Auto Scaling group by 25 percent and you
    -- specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances
    -- and the scaling policy is performed, 25 percent of 4 is 1. However,
    -- because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto
    -- Scaling scales out the group by 2 instances.
    --
    -- Valid only if the policy type is @StepScaling@ or @SimpleScaling@. For
    -- more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types>
    -- in the /Amazon EC2 Auto Scaling User Guide/.
    --
    -- Some Auto Scaling groups use instance weights. In this case, set the
    -- @MinAdjustmentMagnitude@ to a value that is at least as large as your
    -- largest instance weight.
    minAdjustmentMagnitude :: Prelude.Maybe Prelude.Int,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The name of the policy.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutScalingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minAdjustmentStep', 'putScalingPolicy_minAdjustmentStep' - Available for backward compatibility. Use @MinAdjustmentMagnitude@
-- instead.
--
-- 'estimatedInstanceWarmup', 'putScalingPolicy_estimatedInstanceWarmup' - The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics. If not provided, the default is to
-- use the value from the default cooldown period for the Auto Scaling
-- group.
--
-- Valid only if the policy type is @TargetTrackingScaling@ or
-- @StepScaling@.
--
-- 'enabled', 'putScalingPolicy_enabled' - Indicates whether the scaling policy is enabled or disabled. The default
-- is enabled. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'policyType', 'putScalingPolicy_policyType' - One of the following policy types:
--
-- -   @TargetTrackingScaling@
--
-- -   @StepScaling@
--
-- -   @SimpleScaling@ (default)
--
-- -   @PredictiveScaling@
--
-- 'stepAdjustments', 'putScalingPolicy_stepAdjustments' - A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
--
-- Required if the policy type is @StepScaling@. (Not used with any other
-- policy type.)
--
-- 'targetTrackingConfiguration', 'putScalingPolicy_targetTrackingConfiguration' - A target tracking scaling policy. Provides support for predefined or
-- customized metrics.
--
-- The following predefined metrics are available:
--
-- -   @ASGAverageCPUUtilization@
--
-- -   @ASGAverageNetworkIn@
--
-- -   @ASGAverageNetworkOut@
--
-- -   @ALBRequestCountPerTarget@
--
-- If you specify @ALBRequestCountPerTarget@ for the metric, you must
-- specify the @ResourceLabel@ parameter with the
-- @PredefinedMetricSpecification@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_TargetTrackingConfiguration.html TargetTrackingConfiguration>
-- in the /Amazon EC2 Auto Scaling API Reference/.
--
-- Required if the policy type is @TargetTrackingScaling@.
--
-- 'adjustmentType', 'putScalingPolicy_adjustmentType' - Specifies how the scaling adjustment is interpreted (for example, an
-- absolute number or a percentage). The valid values are
-- @ChangeInCapacity@, @ExactCapacity@, and @PercentChangeInCapacity@.
--
-- Required if the policy type is @StepScaling@ or @SimpleScaling@. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'predictiveScalingConfiguration', 'putScalingPolicy_predictiveScalingConfiguration' - A predictive scaling policy. Provides support for only predefined
-- metrics.
--
-- Predictive scaling works with CPU utilization, network in\/out, and the
-- Application Load Balancer request count.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_PredictiveScalingConfiguration.html PredictiveScalingConfiguration>
-- in the /Amazon EC2 Auto Scaling API Reference/.
--
-- Required if the policy type is @PredictiveScaling@.
--
-- 'scalingAdjustment', 'putScalingPolicy_scalingAdjustment' - The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity. For exact capacity, you must specify
-- a positive value.
--
-- Required if the policy type is @SimpleScaling@. (Not used with any other
-- policy type.)
--
-- 'cooldown', 'putScalingPolicy_cooldown' - The duration of the policy\'s cooldown period, in seconds. When a
-- cooldown period is specified here, it overrides the default cooldown
-- period defined for the Auto Scaling group.
--
-- Valid only if the policy type is @SimpleScaling@. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- 'metricAggregationType', 'putScalingPolicy_metricAggregationType' - The aggregation type for the CloudWatch metrics. The valid values are
-- @Minimum@, @Maximum@, and @Average@. If the aggregation type is null,
-- the value is treated as @Average@.
--
-- Valid only if the policy type is @StepScaling@.
--
-- 'minAdjustmentMagnitude', 'putScalingPolicy_minAdjustmentMagnitude' - The minimum value to scale by when the adjustment type is
-- @PercentChangeInCapacity@. For example, suppose that you create a step
-- scaling policy to scale out an Auto Scaling group by 25 percent and you
-- specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances
-- and the scaling policy is performed, 25 percent of 4 is 1. However,
-- because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto
-- Scaling scales out the group by 2 instances.
--
-- Valid only if the policy type is @StepScaling@ or @SimpleScaling@. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Some Auto Scaling groups use instance weights. In this case, set the
-- @MinAdjustmentMagnitude@ to a value that is at least as large as your
-- largest instance weight.
--
-- 'autoScalingGroupName', 'putScalingPolicy_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'policyName', 'putScalingPolicy_policyName' - The name of the policy.
newPutScalingPolicy ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  PutScalingPolicy
newPutScalingPolicy
  pAutoScalingGroupName_
  pPolicyName_ =
    PutScalingPolicy'
      { minAdjustmentStep =
          Prelude.Nothing,
        estimatedInstanceWarmup = Prelude.Nothing,
        enabled = Prelude.Nothing,
        policyType = Prelude.Nothing,
        stepAdjustments = Prelude.Nothing,
        targetTrackingConfiguration = Prelude.Nothing,
        adjustmentType = Prelude.Nothing,
        predictiveScalingConfiguration = Prelude.Nothing,
        scalingAdjustment = Prelude.Nothing,
        cooldown = Prelude.Nothing,
        metricAggregationType = Prelude.Nothing,
        minAdjustmentMagnitude = Prelude.Nothing,
        autoScalingGroupName = pAutoScalingGroupName_,
        policyName = pPolicyName_
      }

-- | Available for backward compatibility. Use @MinAdjustmentMagnitude@
-- instead.
putScalingPolicy_minAdjustmentStep :: Lens.Lens' PutScalingPolicy (Prelude.Maybe Prelude.Int)
putScalingPolicy_minAdjustmentStep = Lens.lens (\PutScalingPolicy' {minAdjustmentStep} -> minAdjustmentStep) (\s@PutScalingPolicy' {} a -> s {minAdjustmentStep = a} :: PutScalingPolicy)

-- | The estimated time, in seconds, until a newly launched instance can
-- contribute to the CloudWatch metrics. If not provided, the default is to
-- use the value from the default cooldown period for the Auto Scaling
-- group.
--
-- Valid only if the policy type is @TargetTrackingScaling@ or
-- @StepScaling@.
putScalingPolicy_estimatedInstanceWarmup :: Lens.Lens' PutScalingPolicy (Prelude.Maybe Prelude.Int)
putScalingPolicy_estimatedInstanceWarmup = Lens.lens (\PutScalingPolicy' {estimatedInstanceWarmup} -> estimatedInstanceWarmup) (\s@PutScalingPolicy' {} a -> s {estimatedInstanceWarmup = a} :: PutScalingPolicy)

-- | Indicates whether the scaling policy is enabled or disabled. The default
-- is enabled. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enable-disable-scaling-policy.html Disabling a scaling policy for an Auto Scaling group>
-- in the /Amazon EC2 Auto Scaling User Guide/.
putScalingPolicy_enabled :: Lens.Lens' PutScalingPolicy (Prelude.Maybe Prelude.Bool)
putScalingPolicy_enabled = Lens.lens (\PutScalingPolicy' {enabled} -> enabled) (\s@PutScalingPolicy' {} a -> s {enabled = a} :: PutScalingPolicy)

-- | One of the following policy types:
--
-- -   @TargetTrackingScaling@
--
-- -   @StepScaling@
--
-- -   @SimpleScaling@ (default)
--
-- -   @PredictiveScaling@
putScalingPolicy_policyType :: Lens.Lens' PutScalingPolicy (Prelude.Maybe Prelude.Text)
putScalingPolicy_policyType = Lens.lens (\PutScalingPolicy' {policyType} -> policyType) (\s@PutScalingPolicy' {} a -> s {policyType = a} :: PutScalingPolicy)

-- | A set of adjustments that enable you to scale based on the size of the
-- alarm breach.
--
-- Required if the policy type is @StepScaling@. (Not used with any other
-- policy type.)
putScalingPolicy_stepAdjustments :: Lens.Lens' PutScalingPolicy (Prelude.Maybe [StepAdjustment])
putScalingPolicy_stepAdjustments = Lens.lens (\PutScalingPolicy' {stepAdjustments} -> stepAdjustments) (\s@PutScalingPolicy' {} a -> s {stepAdjustments = a} :: PutScalingPolicy) Prelude.. Lens.mapping Lens.coerced

-- | A target tracking scaling policy. Provides support for predefined or
-- customized metrics.
--
-- The following predefined metrics are available:
--
-- -   @ASGAverageCPUUtilization@
--
-- -   @ASGAverageNetworkIn@
--
-- -   @ASGAverageNetworkOut@
--
-- -   @ALBRequestCountPerTarget@
--
-- If you specify @ALBRequestCountPerTarget@ for the metric, you must
-- specify the @ResourceLabel@ parameter with the
-- @PredefinedMetricSpecification@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_TargetTrackingConfiguration.html TargetTrackingConfiguration>
-- in the /Amazon EC2 Auto Scaling API Reference/.
--
-- Required if the policy type is @TargetTrackingScaling@.
putScalingPolicy_targetTrackingConfiguration :: Lens.Lens' PutScalingPolicy (Prelude.Maybe TargetTrackingConfiguration)
putScalingPolicy_targetTrackingConfiguration = Lens.lens (\PutScalingPolicy' {targetTrackingConfiguration} -> targetTrackingConfiguration) (\s@PutScalingPolicy' {} a -> s {targetTrackingConfiguration = a} :: PutScalingPolicy)

-- | Specifies how the scaling adjustment is interpreted (for example, an
-- absolute number or a percentage). The valid values are
-- @ChangeInCapacity@, @ExactCapacity@, and @PercentChangeInCapacity@.
--
-- Required if the policy type is @StepScaling@ or @SimpleScaling@. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types>
-- in the /Amazon EC2 Auto Scaling User Guide/.
putScalingPolicy_adjustmentType :: Lens.Lens' PutScalingPolicy (Prelude.Maybe Prelude.Text)
putScalingPolicy_adjustmentType = Lens.lens (\PutScalingPolicy' {adjustmentType} -> adjustmentType) (\s@PutScalingPolicy' {} a -> s {adjustmentType = a} :: PutScalingPolicy)

-- | A predictive scaling policy. Provides support for only predefined
-- metrics.
--
-- Predictive scaling works with CPU utilization, network in\/out, and the
-- Application Load Balancer request count.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_PredictiveScalingConfiguration.html PredictiveScalingConfiguration>
-- in the /Amazon EC2 Auto Scaling API Reference/.
--
-- Required if the policy type is @PredictiveScaling@.
putScalingPolicy_predictiveScalingConfiguration :: Lens.Lens' PutScalingPolicy (Prelude.Maybe PredictiveScalingConfiguration)
putScalingPolicy_predictiveScalingConfiguration = Lens.lens (\PutScalingPolicy' {predictiveScalingConfiguration} -> predictiveScalingConfiguration) (\s@PutScalingPolicy' {} a -> s {predictiveScalingConfiguration = a} :: PutScalingPolicy)

-- | The amount by which to scale, based on the specified adjustment type. A
-- positive value adds to the current capacity while a negative number
-- removes from the current capacity. For exact capacity, you must specify
-- a positive value.
--
-- Required if the policy type is @SimpleScaling@. (Not used with any other
-- policy type.)
putScalingPolicy_scalingAdjustment :: Lens.Lens' PutScalingPolicy (Prelude.Maybe Prelude.Int)
putScalingPolicy_scalingAdjustment = Lens.lens (\PutScalingPolicy' {scalingAdjustment} -> scalingAdjustment) (\s@PutScalingPolicy' {} a -> s {scalingAdjustment = a} :: PutScalingPolicy)

-- | The duration of the policy\'s cooldown period, in seconds. When a
-- cooldown period is specified here, it overrides the default cooldown
-- period defined for the Auto Scaling group.
--
-- Valid only if the policy type is @SimpleScaling@. For more information,
-- see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/Cooldown.html Scaling cooldowns for Amazon EC2 Auto Scaling>
-- in the /Amazon EC2 Auto Scaling User Guide/.
putScalingPolicy_cooldown :: Lens.Lens' PutScalingPolicy (Prelude.Maybe Prelude.Int)
putScalingPolicy_cooldown = Lens.lens (\PutScalingPolicy' {cooldown} -> cooldown) (\s@PutScalingPolicy' {} a -> s {cooldown = a} :: PutScalingPolicy)

-- | The aggregation type for the CloudWatch metrics. The valid values are
-- @Minimum@, @Maximum@, and @Average@. If the aggregation type is null,
-- the value is treated as @Average@.
--
-- Valid only if the policy type is @StepScaling@.
putScalingPolicy_metricAggregationType :: Lens.Lens' PutScalingPolicy (Prelude.Maybe Prelude.Text)
putScalingPolicy_metricAggregationType = Lens.lens (\PutScalingPolicy' {metricAggregationType} -> metricAggregationType) (\s@PutScalingPolicy' {} a -> s {metricAggregationType = a} :: PutScalingPolicy)

-- | The minimum value to scale by when the adjustment type is
-- @PercentChangeInCapacity@. For example, suppose that you create a step
-- scaling policy to scale out an Auto Scaling group by 25 percent and you
-- specify a @MinAdjustmentMagnitude@ of 2. If the group has 4 instances
-- and the scaling policy is performed, 25 percent of 4 is 1. However,
-- because you specified a @MinAdjustmentMagnitude@ of 2, Amazon EC2 Auto
-- Scaling scales out the group by 2 instances.
--
-- Valid only if the policy type is @StepScaling@ or @SimpleScaling@. For
-- more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-scaling-simple-step.html#as-scaling-adjustment Scaling adjustment types>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- Some Auto Scaling groups use instance weights. In this case, set the
-- @MinAdjustmentMagnitude@ to a value that is at least as large as your
-- largest instance weight.
putScalingPolicy_minAdjustmentMagnitude :: Lens.Lens' PutScalingPolicy (Prelude.Maybe Prelude.Int)
putScalingPolicy_minAdjustmentMagnitude = Lens.lens (\PutScalingPolicy' {minAdjustmentMagnitude} -> minAdjustmentMagnitude) (\s@PutScalingPolicy' {} a -> s {minAdjustmentMagnitude = a} :: PutScalingPolicy)

-- | The name of the Auto Scaling group.
putScalingPolicy_autoScalingGroupName :: Lens.Lens' PutScalingPolicy Prelude.Text
putScalingPolicy_autoScalingGroupName = Lens.lens (\PutScalingPolicy' {autoScalingGroupName} -> autoScalingGroupName) (\s@PutScalingPolicy' {} a -> s {autoScalingGroupName = a} :: PutScalingPolicy)

-- | The name of the policy.
putScalingPolicy_policyName :: Lens.Lens' PutScalingPolicy Prelude.Text
putScalingPolicy_policyName = Lens.lens (\PutScalingPolicy' {policyName} -> policyName) (\s@PutScalingPolicy' {} a -> s {policyName = a} :: PutScalingPolicy)

instance Core.AWSRequest PutScalingPolicy where
  type
    AWSResponse PutScalingPolicy =
      PutScalingPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "PutScalingPolicyResult"
      ( \s h x ->
          PutScalingPolicyResponse'
            Prelude.<$> (x Core..@? "PolicyARN")
            Prelude.<*> ( x Core..@? "Alarms" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutScalingPolicy where
  hashWithSalt salt' PutScalingPolicy' {..} =
    salt' `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` minAdjustmentMagnitude
      `Prelude.hashWithSalt` metricAggregationType
      `Prelude.hashWithSalt` cooldown
      `Prelude.hashWithSalt` scalingAdjustment
      `Prelude.hashWithSalt` predictiveScalingConfiguration
      `Prelude.hashWithSalt` adjustmentType
      `Prelude.hashWithSalt` targetTrackingConfiguration
      `Prelude.hashWithSalt` stepAdjustments
      `Prelude.hashWithSalt` policyType
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` estimatedInstanceWarmup
      `Prelude.hashWithSalt` minAdjustmentStep

instance Prelude.NFData PutScalingPolicy where
  rnf PutScalingPolicy' {..} =
    Prelude.rnf minAdjustmentStep
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf minAdjustmentMagnitude
      `Prelude.seq` Prelude.rnf metricAggregationType
      `Prelude.seq` Prelude.rnf cooldown
      `Prelude.seq` Prelude.rnf scalingAdjustment
      `Prelude.seq` Prelude.rnf predictiveScalingConfiguration
      `Prelude.seq` Prelude.rnf adjustmentType
      `Prelude.seq` Prelude.rnf targetTrackingConfiguration
      `Prelude.seq` Prelude.rnf stepAdjustments
      `Prelude.seq` Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf estimatedInstanceWarmup

instance Core.ToHeaders PutScalingPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath PutScalingPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery PutScalingPolicy where
  toQuery PutScalingPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("PutScalingPolicy" :: Prelude.ByteString),
        "Version"
          Core.=: ("2011-01-01" :: Prelude.ByteString),
        "MinAdjustmentStep" Core.=: minAdjustmentStep,
        "EstimatedInstanceWarmup"
          Core.=: estimatedInstanceWarmup,
        "Enabled" Core.=: enabled,
        "PolicyType" Core.=: policyType,
        "StepAdjustments"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> stepAdjustments
            ),
        "TargetTrackingConfiguration"
          Core.=: targetTrackingConfiguration,
        "AdjustmentType" Core.=: adjustmentType,
        "PredictiveScalingConfiguration"
          Core.=: predictiveScalingConfiguration,
        "ScalingAdjustment" Core.=: scalingAdjustment,
        "Cooldown" Core.=: cooldown,
        "MetricAggregationType"
          Core.=: metricAggregationType,
        "MinAdjustmentMagnitude"
          Core.=: minAdjustmentMagnitude,
        "AutoScalingGroupName" Core.=: autoScalingGroupName,
        "PolicyName" Core.=: policyName
      ]

-- | Contains the output of PutScalingPolicy.
--
-- /See:/ 'newPutScalingPolicyResponse' smart constructor.
data PutScalingPolicyResponse = PutScalingPolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the policy.
    policyARN :: Prelude.Maybe Prelude.Text,
    -- | The CloudWatch alarms created for the target tracking scaling policy.
    alarms :: Prelude.Maybe [Alarm],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutScalingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyARN', 'putScalingPolicyResponse_policyARN' - The Amazon Resource Name (ARN) of the policy.
--
-- 'alarms', 'putScalingPolicyResponse_alarms' - The CloudWatch alarms created for the target tracking scaling policy.
--
-- 'httpStatus', 'putScalingPolicyResponse_httpStatus' - The response's http status code.
newPutScalingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutScalingPolicyResponse
newPutScalingPolicyResponse pHttpStatus_ =
  PutScalingPolicyResponse'
    { policyARN =
        Prelude.Nothing,
      alarms = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the policy.
putScalingPolicyResponse_policyARN :: Lens.Lens' PutScalingPolicyResponse (Prelude.Maybe Prelude.Text)
putScalingPolicyResponse_policyARN = Lens.lens (\PutScalingPolicyResponse' {policyARN} -> policyARN) (\s@PutScalingPolicyResponse' {} a -> s {policyARN = a} :: PutScalingPolicyResponse)

-- | The CloudWatch alarms created for the target tracking scaling policy.
putScalingPolicyResponse_alarms :: Lens.Lens' PutScalingPolicyResponse (Prelude.Maybe [Alarm])
putScalingPolicyResponse_alarms = Lens.lens (\PutScalingPolicyResponse' {alarms} -> alarms) (\s@PutScalingPolicyResponse' {} a -> s {alarms = a} :: PutScalingPolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putScalingPolicyResponse_httpStatus :: Lens.Lens' PutScalingPolicyResponse Prelude.Int
putScalingPolicyResponse_httpStatus = Lens.lens (\PutScalingPolicyResponse' {httpStatus} -> httpStatus) (\s@PutScalingPolicyResponse' {} a -> s {httpStatus = a} :: PutScalingPolicyResponse)

instance Prelude.NFData PutScalingPolicyResponse where
  rnf PutScalingPolicyResponse' {..} =
    Prelude.rnf policyARN
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf alarms
