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
-- Module      : Amazonka.AutoScaling.Types.TargetTrackingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.TargetTrackingConfiguration where

import Amazonka.AutoScaling.Types.CustomizedMetricSpecification
import Amazonka.AutoScaling.Types.PredefinedMetricSpecification
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a target tracking scaling policy configuration to use with
-- Amazon EC2 Auto Scaling.
--
-- /See:/ 'newTargetTrackingConfiguration' smart constructor.
data TargetTrackingConfiguration = TargetTrackingConfiguration'
  { -- | A predefined metric. You must specify either a predefined metric or a
    -- customized metric.
    predefinedMetricSpecification :: Prelude.Maybe PredefinedMetricSpecification,
    -- | A customized metric. You must specify either a predefined metric or a
    -- customized metric.
    customizedMetricSpecification :: Prelude.Maybe CustomizedMetricSpecification,
    -- | Indicates whether scaling in by the target tracking scaling policy is
    -- disabled. If scaling in is disabled, the target tracking scaling policy
    -- doesn\'t remove instances from the Auto Scaling group. Otherwise, the
    -- target tracking scaling policy can remove instances from the Auto
    -- Scaling group. The default is @false@.
    disableScaleIn :: Prelude.Maybe Prelude.Bool,
    -- | The target value for the metric.
    targetValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetTrackingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predefinedMetricSpecification', 'targetTrackingConfiguration_predefinedMetricSpecification' - A predefined metric. You must specify either a predefined metric or a
-- customized metric.
--
-- 'customizedMetricSpecification', 'targetTrackingConfiguration_customizedMetricSpecification' - A customized metric. You must specify either a predefined metric or a
-- customized metric.
--
-- 'disableScaleIn', 'targetTrackingConfiguration_disableScaleIn' - Indicates whether scaling in by the target tracking scaling policy is
-- disabled. If scaling in is disabled, the target tracking scaling policy
-- doesn\'t remove instances from the Auto Scaling group. Otherwise, the
-- target tracking scaling policy can remove instances from the Auto
-- Scaling group. The default is @false@.
--
-- 'targetValue', 'targetTrackingConfiguration_targetValue' - The target value for the metric.
newTargetTrackingConfiguration ::
  -- | 'targetValue'
  Prelude.Double ->
  TargetTrackingConfiguration
newTargetTrackingConfiguration pTargetValue_ =
  TargetTrackingConfiguration'
    { predefinedMetricSpecification =
        Prelude.Nothing,
      customizedMetricSpecification =
        Prelude.Nothing,
      disableScaleIn = Prelude.Nothing,
      targetValue = pTargetValue_
    }

-- | A predefined metric. You must specify either a predefined metric or a
-- customized metric.
targetTrackingConfiguration_predefinedMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe PredefinedMetricSpecification)
targetTrackingConfiguration_predefinedMetricSpecification = Lens.lens (\TargetTrackingConfiguration' {predefinedMetricSpecification} -> predefinedMetricSpecification) (\s@TargetTrackingConfiguration' {} a -> s {predefinedMetricSpecification = a} :: TargetTrackingConfiguration)

-- | A customized metric. You must specify either a predefined metric or a
-- customized metric.
targetTrackingConfiguration_customizedMetricSpecification :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe CustomizedMetricSpecification)
targetTrackingConfiguration_customizedMetricSpecification = Lens.lens (\TargetTrackingConfiguration' {customizedMetricSpecification} -> customizedMetricSpecification) (\s@TargetTrackingConfiguration' {} a -> s {customizedMetricSpecification = a} :: TargetTrackingConfiguration)

-- | Indicates whether scaling in by the target tracking scaling policy is
-- disabled. If scaling in is disabled, the target tracking scaling policy
-- doesn\'t remove instances from the Auto Scaling group. Otherwise, the
-- target tracking scaling policy can remove instances from the Auto
-- Scaling group. The default is @false@.
targetTrackingConfiguration_disableScaleIn :: Lens.Lens' TargetTrackingConfiguration (Prelude.Maybe Prelude.Bool)
targetTrackingConfiguration_disableScaleIn = Lens.lens (\TargetTrackingConfiguration' {disableScaleIn} -> disableScaleIn) (\s@TargetTrackingConfiguration' {} a -> s {disableScaleIn = a} :: TargetTrackingConfiguration)

-- | The target value for the metric.
targetTrackingConfiguration_targetValue :: Lens.Lens' TargetTrackingConfiguration Prelude.Double
targetTrackingConfiguration_targetValue = Lens.lens (\TargetTrackingConfiguration' {targetValue} -> targetValue) (\s@TargetTrackingConfiguration' {} a -> s {targetValue = a} :: TargetTrackingConfiguration)

instance Core.FromXML TargetTrackingConfiguration where
  parseXML x =
    TargetTrackingConfiguration'
      Prelude.<$> (x Core..@? "PredefinedMetricSpecification")
      Prelude.<*> (x Core..@? "CustomizedMetricSpecification")
      Prelude.<*> (x Core..@? "DisableScaleIn")
      Prelude.<*> (x Core..@ "TargetValue")

instance Prelude.Hashable TargetTrackingConfiguration where
  hashWithSalt salt' TargetTrackingConfiguration' {..} =
    salt' `Prelude.hashWithSalt` targetValue
      `Prelude.hashWithSalt` disableScaleIn
      `Prelude.hashWithSalt` customizedMetricSpecification
      `Prelude.hashWithSalt` predefinedMetricSpecification

instance Prelude.NFData TargetTrackingConfiguration where
  rnf TargetTrackingConfiguration' {..} =
    Prelude.rnf predefinedMetricSpecification
      `Prelude.seq` Prelude.rnf targetValue
      `Prelude.seq` Prelude.rnf disableScaleIn
      `Prelude.seq` Prelude.rnf customizedMetricSpecification

instance Core.ToQuery TargetTrackingConfiguration where
  toQuery TargetTrackingConfiguration' {..} =
    Prelude.mconcat
      [ "PredefinedMetricSpecification"
          Core.=: predefinedMetricSpecification,
        "CustomizedMetricSpecification"
          Core.=: customizedMetricSpecification,
        "DisableScaleIn" Core.=: disableScaleIn,
        "TargetValue" Core.=: targetValue
      ]
