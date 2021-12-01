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
-- Module      : Amazonka.Proton.Types.EnvironmentTemplateVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentTemplateVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.TemplateVersionStatus

-- | A summary of the version of an environment template detail data.
--
-- /See:/ 'newEnvironmentTemplateVersionSummary' smart constructor.
data EnvironmentTemplateVersionSummary = EnvironmentTemplateVersionSummary'
  { -- | The status message of the version of an environment template.
    statusMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the recommended minor version of the environment template.
    recommendedMinorVersion :: Prelude.Maybe Prelude.Text,
    -- | A description of the version of an environment template.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the version of an environment
    -- template.
    arn :: Prelude.Text,
    -- | The time when the version of an environment template was created.
    createdAt :: Core.POSIX,
    -- | The time when the version of an environment template was last modified.
    lastModifiedAt :: Core.POSIX,
    -- | The ID of the latest major version that\'s associated with the version
    -- of an environment template.
    majorVersion :: Prelude.Text,
    -- | The ID of the version of an environment template.
    minorVersion :: Prelude.Text,
    -- | The status of the version of an environment template.
    status :: TemplateVersionStatus,
    -- | The name of the version of an environment template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentTemplateVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'environmentTemplateVersionSummary_statusMessage' - The status message of the version of an environment template.
--
-- 'recommendedMinorVersion', 'environmentTemplateVersionSummary_recommendedMinorVersion' - The ID of the recommended minor version of the environment template.
--
-- 'description', 'environmentTemplateVersionSummary_description' - A description of the version of an environment template.
--
-- 'arn', 'environmentTemplateVersionSummary_arn' - The Amazon Resource Name (ARN) of the version of an environment
-- template.
--
-- 'createdAt', 'environmentTemplateVersionSummary_createdAt' - The time when the version of an environment template was created.
--
-- 'lastModifiedAt', 'environmentTemplateVersionSummary_lastModifiedAt' - The time when the version of an environment template was last modified.
--
-- 'majorVersion', 'environmentTemplateVersionSummary_majorVersion' - The ID of the latest major version that\'s associated with the version
-- of an environment template.
--
-- 'minorVersion', 'environmentTemplateVersionSummary_minorVersion' - The ID of the version of an environment template.
--
-- 'status', 'environmentTemplateVersionSummary_status' - The status of the version of an environment template.
--
-- 'templateName', 'environmentTemplateVersionSummary_templateName' - The name of the version of an environment template.
newEnvironmentTemplateVersionSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'minorVersion'
  Prelude.Text ->
  -- | 'status'
  TemplateVersionStatus ->
  -- | 'templateName'
  Prelude.Text ->
  EnvironmentTemplateVersionSummary
newEnvironmentTemplateVersionSummary
  pArn_
  pCreatedAt_
  pLastModifiedAt_
  pMajorVersion_
  pMinorVersion_
  pStatus_
  pTemplateName_ =
    EnvironmentTemplateVersionSummary'
      { statusMessage =
          Prelude.Nothing,
        recommendedMinorVersion =
          Prelude.Nothing,
        description = Prelude.Nothing,
        arn = pArn_,
        createdAt =
          Core._Time Lens.# pCreatedAt_,
        lastModifiedAt =
          Core._Time Lens.# pLastModifiedAt_,
        majorVersion = pMajorVersion_,
        minorVersion = pMinorVersion_,
        status = pStatus_,
        templateName = pTemplateName_
      }

-- | The status message of the version of an environment template.
environmentTemplateVersionSummary_statusMessage :: Lens.Lens' EnvironmentTemplateVersionSummary (Prelude.Maybe Prelude.Text)
environmentTemplateVersionSummary_statusMessage = Lens.lens (\EnvironmentTemplateVersionSummary' {statusMessage} -> statusMessage) (\s@EnvironmentTemplateVersionSummary' {} a -> s {statusMessage = a} :: EnvironmentTemplateVersionSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the recommended minor version of the environment template.
environmentTemplateVersionSummary_recommendedMinorVersion :: Lens.Lens' EnvironmentTemplateVersionSummary (Prelude.Maybe Prelude.Text)
environmentTemplateVersionSummary_recommendedMinorVersion = Lens.lens (\EnvironmentTemplateVersionSummary' {recommendedMinorVersion} -> recommendedMinorVersion) (\s@EnvironmentTemplateVersionSummary' {} a -> s {recommendedMinorVersion = a} :: EnvironmentTemplateVersionSummary)

-- | A description of the version of an environment template.
environmentTemplateVersionSummary_description :: Lens.Lens' EnvironmentTemplateVersionSummary (Prelude.Maybe Prelude.Text)
environmentTemplateVersionSummary_description = Lens.lens (\EnvironmentTemplateVersionSummary' {description} -> description) (\s@EnvironmentTemplateVersionSummary' {} a -> s {description = a} :: EnvironmentTemplateVersionSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the version of an environment
-- template.
environmentTemplateVersionSummary_arn :: Lens.Lens' EnvironmentTemplateVersionSummary Prelude.Text
environmentTemplateVersionSummary_arn = Lens.lens (\EnvironmentTemplateVersionSummary' {arn} -> arn) (\s@EnvironmentTemplateVersionSummary' {} a -> s {arn = a} :: EnvironmentTemplateVersionSummary)

-- | The time when the version of an environment template was created.
environmentTemplateVersionSummary_createdAt :: Lens.Lens' EnvironmentTemplateVersionSummary Prelude.UTCTime
environmentTemplateVersionSummary_createdAt = Lens.lens (\EnvironmentTemplateVersionSummary' {createdAt} -> createdAt) (\s@EnvironmentTemplateVersionSummary' {} a -> s {createdAt = a} :: EnvironmentTemplateVersionSummary) Prelude.. Core._Time

-- | The time when the version of an environment template was last modified.
environmentTemplateVersionSummary_lastModifiedAt :: Lens.Lens' EnvironmentTemplateVersionSummary Prelude.UTCTime
environmentTemplateVersionSummary_lastModifiedAt = Lens.lens (\EnvironmentTemplateVersionSummary' {lastModifiedAt} -> lastModifiedAt) (\s@EnvironmentTemplateVersionSummary' {} a -> s {lastModifiedAt = a} :: EnvironmentTemplateVersionSummary) Prelude.. Core._Time

-- | The ID of the latest major version that\'s associated with the version
-- of an environment template.
environmentTemplateVersionSummary_majorVersion :: Lens.Lens' EnvironmentTemplateVersionSummary Prelude.Text
environmentTemplateVersionSummary_majorVersion = Lens.lens (\EnvironmentTemplateVersionSummary' {majorVersion} -> majorVersion) (\s@EnvironmentTemplateVersionSummary' {} a -> s {majorVersion = a} :: EnvironmentTemplateVersionSummary)

-- | The ID of the version of an environment template.
environmentTemplateVersionSummary_minorVersion :: Lens.Lens' EnvironmentTemplateVersionSummary Prelude.Text
environmentTemplateVersionSummary_minorVersion = Lens.lens (\EnvironmentTemplateVersionSummary' {minorVersion} -> minorVersion) (\s@EnvironmentTemplateVersionSummary' {} a -> s {minorVersion = a} :: EnvironmentTemplateVersionSummary)

-- | The status of the version of an environment template.
environmentTemplateVersionSummary_status :: Lens.Lens' EnvironmentTemplateVersionSummary TemplateVersionStatus
environmentTemplateVersionSummary_status = Lens.lens (\EnvironmentTemplateVersionSummary' {status} -> status) (\s@EnvironmentTemplateVersionSummary' {} a -> s {status = a} :: EnvironmentTemplateVersionSummary)

-- | The name of the version of an environment template.
environmentTemplateVersionSummary_templateName :: Lens.Lens' EnvironmentTemplateVersionSummary Prelude.Text
environmentTemplateVersionSummary_templateName = Lens.lens (\EnvironmentTemplateVersionSummary' {templateName} -> templateName) (\s@EnvironmentTemplateVersionSummary' {} a -> s {templateName = a} :: EnvironmentTemplateVersionSummary)

instance
  Core.FromJSON
    EnvironmentTemplateVersionSummary
  where
  parseJSON =
    Core.withObject
      "EnvironmentTemplateVersionSummary"
      ( \x ->
          EnvironmentTemplateVersionSummary'
            Prelude.<$> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "recommendedMinorVersion")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "lastModifiedAt")
            Prelude.<*> (x Core..: "majorVersion")
            Prelude.<*> (x Core..: "minorVersion")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "templateName")
      )

instance
  Prelude.Hashable
    EnvironmentTemplateVersionSummary
  where
  hashWithSalt
    salt'
    EnvironmentTemplateVersionSummary' {..} =
      salt' `Prelude.hashWithSalt` templateName
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` minorVersion
        `Prelude.hashWithSalt` majorVersion
        `Prelude.hashWithSalt` lastModifiedAt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` recommendedMinorVersion
        `Prelude.hashWithSalt` statusMessage

instance
  Prelude.NFData
    EnvironmentTemplateVersionSummary
  where
  rnf EnvironmentTemplateVersionSummary' {..} =
    Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf recommendedMinorVersion
