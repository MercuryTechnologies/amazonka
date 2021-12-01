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
-- Module      : Amazonka.LookoutMetrics.Types.Alert
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.Alert where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types.Action
import Amazonka.LookoutMetrics.Types.AlertStatus
import Amazonka.LookoutMetrics.Types.AlertType
import qualified Amazonka.Prelude as Prelude

-- | A configuration for Amazon SNS-integrated notifications.
--
-- /See:/ 'newAlert' smart constructor.
data Alert = Alert'
  { -- | The time at which the alert was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | Action that will be triggered when there is an alert.
    action :: Prelude.Maybe Action,
    -- | The ARN of the detector to which the alert is attached.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the alert.
    alertName :: Prelude.Maybe Prelude.Text,
    -- | The minimum severity for an anomaly to trigger the alert.
    alertSensitivityThreshold :: Prelude.Maybe Prelude.Natural,
    -- | The status of the alert.
    alertStatus :: Prelude.Maybe AlertStatus,
    -- | A description of the alert.
    alertDescription :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the alert.
    alertArn :: Prelude.Maybe Prelude.Text,
    -- | The type of the alert.
    alertType :: Prelude.Maybe AlertType,
    -- | The time at which the alert was last modified.
    lastModificationTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Alert' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'alert_creationTime' - The time at which the alert was created.
--
-- 'action', 'alert_action' - Action that will be triggered when there is an alert.
--
-- 'anomalyDetectorArn', 'alert_anomalyDetectorArn' - The ARN of the detector to which the alert is attached.
--
-- 'alertName', 'alert_alertName' - The name of the alert.
--
-- 'alertSensitivityThreshold', 'alert_alertSensitivityThreshold' - The minimum severity for an anomaly to trigger the alert.
--
-- 'alertStatus', 'alert_alertStatus' - The status of the alert.
--
-- 'alertDescription', 'alert_alertDescription' - A description of the alert.
--
-- 'alertArn', 'alert_alertArn' - The ARN of the alert.
--
-- 'alertType', 'alert_alertType' - The type of the alert.
--
-- 'lastModificationTime', 'alert_lastModificationTime' - The time at which the alert was last modified.
newAlert ::
  Alert
newAlert =
  Alert'
    { creationTime = Prelude.Nothing,
      action = Prelude.Nothing,
      anomalyDetectorArn = Prelude.Nothing,
      alertName = Prelude.Nothing,
      alertSensitivityThreshold = Prelude.Nothing,
      alertStatus = Prelude.Nothing,
      alertDescription = Prelude.Nothing,
      alertArn = Prelude.Nothing,
      alertType = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing
    }

-- | The time at which the alert was created.
alert_creationTime :: Lens.Lens' Alert (Prelude.Maybe Prelude.UTCTime)
alert_creationTime = Lens.lens (\Alert' {creationTime} -> creationTime) (\s@Alert' {} a -> s {creationTime = a} :: Alert) Prelude.. Lens.mapping Core._Time

-- | Action that will be triggered when there is an alert.
alert_action :: Lens.Lens' Alert (Prelude.Maybe Action)
alert_action = Lens.lens (\Alert' {action} -> action) (\s@Alert' {} a -> s {action = a} :: Alert)

-- | The ARN of the detector to which the alert is attached.
alert_anomalyDetectorArn :: Lens.Lens' Alert (Prelude.Maybe Prelude.Text)
alert_anomalyDetectorArn = Lens.lens (\Alert' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@Alert' {} a -> s {anomalyDetectorArn = a} :: Alert)

-- | The name of the alert.
alert_alertName :: Lens.Lens' Alert (Prelude.Maybe Prelude.Text)
alert_alertName = Lens.lens (\Alert' {alertName} -> alertName) (\s@Alert' {} a -> s {alertName = a} :: Alert)

-- | The minimum severity for an anomaly to trigger the alert.
alert_alertSensitivityThreshold :: Lens.Lens' Alert (Prelude.Maybe Prelude.Natural)
alert_alertSensitivityThreshold = Lens.lens (\Alert' {alertSensitivityThreshold} -> alertSensitivityThreshold) (\s@Alert' {} a -> s {alertSensitivityThreshold = a} :: Alert)

-- | The status of the alert.
alert_alertStatus :: Lens.Lens' Alert (Prelude.Maybe AlertStatus)
alert_alertStatus = Lens.lens (\Alert' {alertStatus} -> alertStatus) (\s@Alert' {} a -> s {alertStatus = a} :: Alert)

-- | A description of the alert.
alert_alertDescription :: Lens.Lens' Alert (Prelude.Maybe Prelude.Text)
alert_alertDescription = Lens.lens (\Alert' {alertDescription} -> alertDescription) (\s@Alert' {} a -> s {alertDescription = a} :: Alert)

-- | The ARN of the alert.
alert_alertArn :: Lens.Lens' Alert (Prelude.Maybe Prelude.Text)
alert_alertArn = Lens.lens (\Alert' {alertArn} -> alertArn) (\s@Alert' {} a -> s {alertArn = a} :: Alert)

-- | The type of the alert.
alert_alertType :: Lens.Lens' Alert (Prelude.Maybe AlertType)
alert_alertType = Lens.lens (\Alert' {alertType} -> alertType) (\s@Alert' {} a -> s {alertType = a} :: Alert)

-- | The time at which the alert was last modified.
alert_lastModificationTime :: Lens.Lens' Alert (Prelude.Maybe Prelude.UTCTime)
alert_lastModificationTime = Lens.lens (\Alert' {lastModificationTime} -> lastModificationTime) (\s@Alert' {} a -> s {lastModificationTime = a} :: Alert) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Alert where
  parseJSON =
    Core.withObject
      "Alert"
      ( \x ->
          Alert'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Action")
            Prelude.<*> (x Core..:? "AnomalyDetectorArn")
            Prelude.<*> (x Core..:? "AlertName")
            Prelude.<*> (x Core..:? "AlertSensitivityThreshold")
            Prelude.<*> (x Core..:? "AlertStatus")
            Prelude.<*> (x Core..:? "AlertDescription")
            Prelude.<*> (x Core..:? "AlertArn")
            Prelude.<*> (x Core..:? "AlertType")
            Prelude.<*> (x Core..:? "LastModificationTime")
      )

instance Prelude.Hashable Alert where
  hashWithSalt salt' Alert' {..} =
    salt' `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` alertType
      `Prelude.hashWithSalt` alertArn
      `Prelude.hashWithSalt` alertDescription
      `Prelude.hashWithSalt` alertStatus
      `Prelude.hashWithSalt` alertSensitivityThreshold
      `Prelude.hashWithSalt` alertName
      `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData Alert where
  rnf Alert' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf alertType
      `Prelude.seq` Prelude.rnf alertArn
      `Prelude.seq` Prelude.rnf alertDescription
      `Prelude.seq` Prelude.rnf alertStatus
      `Prelude.seq` Prelude.rnf alertSensitivityThreshold
      `Prelude.seq` Prelude.rnf alertName
      `Prelude.seq` Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf action
