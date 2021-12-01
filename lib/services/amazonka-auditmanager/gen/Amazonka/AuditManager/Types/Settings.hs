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
-- Module      : Amazonka.AuditManager.Types.Settings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Settings where

import Amazonka.AuditManager.Types.AssessmentReportsDestination
import Amazonka.AuditManager.Types.Role
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The settings object that holds all supported Audit Manager settings.
--
-- /See:/ 'newSettings' smart constructor.
data Settings = Settings'
  { -- | The KMS key details.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The default storage destination for assessment reports.
    defaultAssessmentReportsDestination :: Prelude.Maybe AssessmentReportsDestination,
    -- | The designated Amazon Simple Notification Service (Amazon SNS) topic.
    snsTopic :: Prelude.Maybe Prelude.Text,
    -- | The designated default audit owners.
    defaultProcessOwners :: Prelude.Maybe [Role],
    -- | Specifies whether Organizations is enabled.
    isAwsOrgEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Settings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKey', 'settings_kmsKey' - The KMS key details.
--
-- 'defaultAssessmentReportsDestination', 'settings_defaultAssessmentReportsDestination' - The default storage destination for assessment reports.
--
-- 'snsTopic', 'settings_snsTopic' - The designated Amazon Simple Notification Service (Amazon SNS) topic.
--
-- 'defaultProcessOwners', 'settings_defaultProcessOwners' - The designated default audit owners.
--
-- 'isAwsOrgEnabled', 'settings_isAwsOrgEnabled' - Specifies whether Organizations is enabled.
newSettings ::
  Settings
newSettings =
  Settings'
    { kmsKey = Prelude.Nothing,
      defaultAssessmentReportsDestination =
        Prelude.Nothing,
      snsTopic = Prelude.Nothing,
      defaultProcessOwners = Prelude.Nothing,
      isAwsOrgEnabled = Prelude.Nothing
    }

-- | The KMS key details.
settings_kmsKey :: Lens.Lens' Settings (Prelude.Maybe Prelude.Text)
settings_kmsKey = Lens.lens (\Settings' {kmsKey} -> kmsKey) (\s@Settings' {} a -> s {kmsKey = a} :: Settings)

-- | The default storage destination for assessment reports.
settings_defaultAssessmentReportsDestination :: Lens.Lens' Settings (Prelude.Maybe AssessmentReportsDestination)
settings_defaultAssessmentReportsDestination = Lens.lens (\Settings' {defaultAssessmentReportsDestination} -> defaultAssessmentReportsDestination) (\s@Settings' {} a -> s {defaultAssessmentReportsDestination = a} :: Settings)

-- | The designated Amazon Simple Notification Service (Amazon SNS) topic.
settings_snsTopic :: Lens.Lens' Settings (Prelude.Maybe Prelude.Text)
settings_snsTopic = Lens.lens (\Settings' {snsTopic} -> snsTopic) (\s@Settings' {} a -> s {snsTopic = a} :: Settings)

-- | The designated default audit owners.
settings_defaultProcessOwners :: Lens.Lens' Settings (Prelude.Maybe [Role])
settings_defaultProcessOwners = Lens.lens (\Settings' {defaultProcessOwners} -> defaultProcessOwners) (\s@Settings' {} a -> s {defaultProcessOwners = a} :: Settings) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether Organizations is enabled.
settings_isAwsOrgEnabled :: Lens.Lens' Settings (Prelude.Maybe Prelude.Bool)
settings_isAwsOrgEnabled = Lens.lens (\Settings' {isAwsOrgEnabled} -> isAwsOrgEnabled) (\s@Settings' {} a -> s {isAwsOrgEnabled = a} :: Settings)

instance Core.FromJSON Settings where
  parseJSON =
    Core.withObject
      "Settings"
      ( \x ->
          Settings'
            Prelude.<$> (x Core..:? "kmsKey")
            Prelude.<*> (x Core..:? "defaultAssessmentReportsDestination")
            Prelude.<*> (x Core..:? "snsTopic")
            Prelude.<*> ( x Core..:? "defaultProcessOwners"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "isAwsOrgEnabled")
      )

instance Prelude.Hashable Settings where
  hashWithSalt salt' Settings' {..} =
    salt' `Prelude.hashWithSalt` isAwsOrgEnabled
      `Prelude.hashWithSalt` defaultProcessOwners
      `Prelude.hashWithSalt` snsTopic
      `Prelude.hashWithSalt` defaultAssessmentReportsDestination
      `Prelude.hashWithSalt` kmsKey

instance Prelude.NFData Settings where
  rnf Settings' {..} =
    Prelude.rnf kmsKey
      `Prelude.seq` Prelude.rnf isAwsOrgEnabled
      `Prelude.seq` Prelude.rnf defaultProcessOwners
      `Prelude.seq` Prelude.rnf snsTopic
      `Prelude.seq` Prelude.rnf defaultAssessmentReportsDestination
