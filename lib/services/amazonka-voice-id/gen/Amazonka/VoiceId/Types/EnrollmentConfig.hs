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
-- Module      : Amazonka.VoiceId.Types.EnrollmentConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.EnrollmentConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.EnrollmentJobFraudDetectionConfig
import Amazonka.VoiceId.Types.ExistingEnrollmentAction

-- | Contains configurations defining enrollment behavior for the batch job.
--
-- /See:/ 'newEnrollmentConfig' smart constructor.
data EnrollmentConfig = EnrollmentConfig'
  { -- | The fraud detection configuration to use for the speaker enrollment job.
    fraudDetectionConfig :: Prelude.Maybe EnrollmentJobFraudDetectionConfig,
    -- | The action to take when the specified speaker is already enrolled in the
    -- specified domain. The default value is @SKIP@, which skips the
    -- enrollment for the existing speaker. Setting the value to @OVERWRITE@
    -- replaces the existing voice prints and enrollment audio stored for that
    -- speaker with new data generated from the latest audio.
    existingEnrollmentAction :: Prelude.Maybe ExistingEnrollmentAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnrollmentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fraudDetectionConfig', 'enrollmentConfig_fraudDetectionConfig' - The fraud detection configuration to use for the speaker enrollment job.
--
-- 'existingEnrollmentAction', 'enrollmentConfig_existingEnrollmentAction' - The action to take when the specified speaker is already enrolled in the
-- specified domain. The default value is @SKIP@, which skips the
-- enrollment for the existing speaker. Setting the value to @OVERWRITE@
-- replaces the existing voice prints and enrollment audio stored for that
-- speaker with new data generated from the latest audio.
newEnrollmentConfig ::
  EnrollmentConfig
newEnrollmentConfig =
  EnrollmentConfig'
    { fraudDetectionConfig =
        Prelude.Nothing,
      existingEnrollmentAction = Prelude.Nothing
    }

-- | The fraud detection configuration to use for the speaker enrollment job.
enrollmentConfig_fraudDetectionConfig :: Lens.Lens' EnrollmentConfig (Prelude.Maybe EnrollmentJobFraudDetectionConfig)
enrollmentConfig_fraudDetectionConfig = Lens.lens (\EnrollmentConfig' {fraudDetectionConfig} -> fraudDetectionConfig) (\s@EnrollmentConfig' {} a -> s {fraudDetectionConfig = a} :: EnrollmentConfig)

-- | The action to take when the specified speaker is already enrolled in the
-- specified domain. The default value is @SKIP@, which skips the
-- enrollment for the existing speaker. Setting the value to @OVERWRITE@
-- replaces the existing voice prints and enrollment audio stored for that
-- speaker with new data generated from the latest audio.
enrollmentConfig_existingEnrollmentAction :: Lens.Lens' EnrollmentConfig (Prelude.Maybe ExistingEnrollmentAction)
enrollmentConfig_existingEnrollmentAction = Lens.lens (\EnrollmentConfig' {existingEnrollmentAction} -> existingEnrollmentAction) (\s@EnrollmentConfig' {} a -> s {existingEnrollmentAction = a} :: EnrollmentConfig)

instance Core.FromJSON EnrollmentConfig where
  parseJSON =
    Core.withObject
      "EnrollmentConfig"
      ( \x ->
          EnrollmentConfig'
            Prelude.<$> (x Core..:? "FraudDetectionConfig")
            Prelude.<*> (x Core..:? "ExistingEnrollmentAction")
      )

instance Prelude.Hashable EnrollmentConfig where
  hashWithSalt salt' EnrollmentConfig' {..} =
    salt'
      `Prelude.hashWithSalt` existingEnrollmentAction
      `Prelude.hashWithSalt` fraudDetectionConfig

instance Prelude.NFData EnrollmentConfig where
  rnf EnrollmentConfig' {..} =
    Prelude.rnf fraudDetectionConfig
      `Prelude.seq` Prelude.rnf existingEnrollmentAction

instance Core.ToJSON EnrollmentConfig where
  toJSON EnrollmentConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FraudDetectionConfig" Core..=)
              Prelude.<$> fraudDetectionConfig,
            ("ExistingEnrollmentAction" Core..=)
              Prelude.<$> existingEnrollmentAction
          ]
      )
