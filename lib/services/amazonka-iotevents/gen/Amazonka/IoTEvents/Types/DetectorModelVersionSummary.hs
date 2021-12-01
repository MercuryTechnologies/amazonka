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
-- Module      : Amazonka.IoTEvents.Types.DetectorModelVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.DetectorModelVersionSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoTEvents.Types.DetectorModelVersionStatus
import Amazonka.IoTEvents.Types.EvaluationMethod
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the detector model version.
--
-- /See:/ 'newDetectorModelVersionSummary' smart constructor.
data DetectorModelVersionSummary = DetectorModelVersionSummary'
  { -- | The time the detector model version was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the detector model version.
    status :: Prelude.Maybe DetectorModelVersionStatus,
    -- | The name of the detector model.
    detectorModelName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the detector model version.
    detectorModelArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the detector model version.
    detectorModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The last time the detector model version was updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | Information about the order in which events are evaluated and how
    -- actions are executed.
    evaluationMethod :: Prelude.Maybe EvaluationMethod,
    -- | The ARN of the role that grants the detector model permission to perform
    -- its tasks.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorModelVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'detectorModelVersionSummary_creationTime' - The time the detector model version was created.
--
-- 'status', 'detectorModelVersionSummary_status' - The status of the detector model version.
--
-- 'detectorModelName', 'detectorModelVersionSummary_detectorModelName' - The name of the detector model.
--
-- 'detectorModelArn', 'detectorModelVersionSummary_detectorModelArn' - The ARN of the detector model version.
--
-- 'detectorModelVersion', 'detectorModelVersionSummary_detectorModelVersion' - The ID of the detector model version.
--
-- 'lastUpdateTime', 'detectorModelVersionSummary_lastUpdateTime' - The last time the detector model version was updated.
--
-- 'evaluationMethod', 'detectorModelVersionSummary_evaluationMethod' - Information about the order in which events are evaluated and how
-- actions are executed.
--
-- 'roleArn', 'detectorModelVersionSummary_roleArn' - The ARN of the role that grants the detector model permission to perform
-- its tasks.
newDetectorModelVersionSummary ::
  DetectorModelVersionSummary
newDetectorModelVersionSummary =
  DetectorModelVersionSummary'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      detectorModelName = Prelude.Nothing,
      detectorModelArn = Prelude.Nothing,
      detectorModelVersion = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      evaluationMethod = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The time the detector model version was created.
detectorModelVersionSummary_creationTime :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.UTCTime)
detectorModelVersionSummary_creationTime = Lens.lens (\DetectorModelVersionSummary' {creationTime} -> creationTime) (\s@DetectorModelVersionSummary' {} a -> s {creationTime = a} :: DetectorModelVersionSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the detector model version.
detectorModelVersionSummary_status :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe DetectorModelVersionStatus)
detectorModelVersionSummary_status = Lens.lens (\DetectorModelVersionSummary' {status} -> status) (\s@DetectorModelVersionSummary' {} a -> s {status = a} :: DetectorModelVersionSummary)

-- | The name of the detector model.
detectorModelVersionSummary_detectorModelName :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.Text)
detectorModelVersionSummary_detectorModelName = Lens.lens (\DetectorModelVersionSummary' {detectorModelName} -> detectorModelName) (\s@DetectorModelVersionSummary' {} a -> s {detectorModelName = a} :: DetectorModelVersionSummary)

-- | The ARN of the detector model version.
detectorModelVersionSummary_detectorModelArn :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.Text)
detectorModelVersionSummary_detectorModelArn = Lens.lens (\DetectorModelVersionSummary' {detectorModelArn} -> detectorModelArn) (\s@DetectorModelVersionSummary' {} a -> s {detectorModelArn = a} :: DetectorModelVersionSummary)

-- | The ID of the detector model version.
detectorModelVersionSummary_detectorModelVersion :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.Text)
detectorModelVersionSummary_detectorModelVersion = Lens.lens (\DetectorModelVersionSummary' {detectorModelVersion} -> detectorModelVersion) (\s@DetectorModelVersionSummary' {} a -> s {detectorModelVersion = a} :: DetectorModelVersionSummary)

-- | The last time the detector model version was updated.
detectorModelVersionSummary_lastUpdateTime :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.UTCTime)
detectorModelVersionSummary_lastUpdateTime = Lens.lens (\DetectorModelVersionSummary' {lastUpdateTime} -> lastUpdateTime) (\s@DetectorModelVersionSummary' {} a -> s {lastUpdateTime = a} :: DetectorModelVersionSummary) Prelude.. Lens.mapping Core._Time

-- | Information about the order in which events are evaluated and how
-- actions are executed.
detectorModelVersionSummary_evaluationMethod :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe EvaluationMethod)
detectorModelVersionSummary_evaluationMethod = Lens.lens (\DetectorModelVersionSummary' {evaluationMethod} -> evaluationMethod) (\s@DetectorModelVersionSummary' {} a -> s {evaluationMethod = a} :: DetectorModelVersionSummary)

-- | The ARN of the role that grants the detector model permission to perform
-- its tasks.
detectorModelVersionSummary_roleArn :: Lens.Lens' DetectorModelVersionSummary (Prelude.Maybe Prelude.Text)
detectorModelVersionSummary_roleArn = Lens.lens (\DetectorModelVersionSummary' {roleArn} -> roleArn) (\s@DetectorModelVersionSummary' {} a -> s {roleArn = a} :: DetectorModelVersionSummary)

instance Core.FromJSON DetectorModelVersionSummary where
  parseJSON =
    Core.withObject
      "DetectorModelVersionSummary"
      ( \x ->
          DetectorModelVersionSummary'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "detectorModelName")
            Prelude.<*> (x Core..:? "detectorModelArn")
            Prelude.<*> (x Core..:? "detectorModelVersion")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "evaluationMethod")
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable DetectorModelVersionSummary where
  hashWithSalt salt' DetectorModelVersionSummary' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` evaluationMethod
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` detectorModelVersion
      `Prelude.hashWithSalt` detectorModelArn
      `Prelude.hashWithSalt` detectorModelName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData DetectorModelVersionSummary where
  rnf DetectorModelVersionSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf evaluationMethod
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf detectorModelVersion
      `Prelude.seq` Prelude.rnf detectorModelArn
      `Prelude.seq` Prelude.rnf detectorModelName
      `Prelude.seq` Prelude.rnf status
