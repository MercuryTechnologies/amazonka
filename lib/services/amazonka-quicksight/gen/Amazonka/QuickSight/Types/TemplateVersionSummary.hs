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
-- Module      : Amazonka.QuickSight.Types.TemplateVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ResourceStatus

-- | The template version.
--
-- /See:/ 'newTemplateVersionSummary' smart constructor.
data TemplateVersionSummary = TemplateVersionSummary'
  { -- | The status of the template version.
    status :: Prelude.Maybe ResourceStatus,
    -- | The Amazon Resource Name (ARN) of the template version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that this template version was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The version number of the template version.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The description of the template version.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'templateVersionSummary_status' - The status of the template version.
--
-- 'arn', 'templateVersionSummary_arn' - The Amazon Resource Name (ARN) of the template version.
--
-- 'createdTime', 'templateVersionSummary_createdTime' - The time that this template version was created.
--
-- 'versionNumber', 'templateVersionSummary_versionNumber' - The version number of the template version.
--
-- 'description', 'templateVersionSummary_description' - The description of the template version.
newTemplateVersionSummary ::
  TemplateVersionSummary
newTemplateVersionSummary =
  TemplateVersionSummary'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The status of the template version.
templateVersionSummary_status :: Lens.Lens' TemplateVersionSummary (Prelude.Maybe ResourceStatus)
templateVersionSummary_status = Lens.lens (\TemplateVersionSummary' {status} -> status) (\s@TemplateVersionSummary' {} a -> s {status = a} :: TemplateVersionSummary)

-- | The Amazon Resource Name (ARN) of the template version.
templateVersionSummary_arn :: Lens.Lens' TemplateVersionSummary (Prelude.Maybe Prelude.Text)
templateVersionSummary_arn = Lens.lens (\TemplateVersionSummary' {arn} -> arn) (\s@TemplateVersionSummary' {} a -> s {arn = a} :: TemplateVersionSummary)

-- | The time that this template version was created.
templateVersionSummary_createdTime :: Lens.Lens' TemplateVersionSummary (Prelude.Maybe Prelude.UTCTime)
templateVersionSummary_createdTime = Lens.lens (\TemplateVersionSummary' {createdTime} -> createdTime) (\s@TemplateVersionSummary' {} a -> s {createdTime = a} :: TemplateVersionSummary) Prelude.. Lens.mapping Core._Time

-- | The version number of the template version.
templateVersionSummary_versionNumber :: Lens.Lens' TemplateVersionSummary (Prelude.Maybe Prelude.Natural)
templateVersionSummary_versionNumber = Lens.lens (\TemplateVersionSummary' {versionNumber} -> versionNumber) (\s@TemplateVersionSummary' {} a -> s {versionNumber = a} :: TemplateVersionSummary)

-- | The description of the template version.
templateVersionSummary_description :: Lens.Lens' TemplateVersionSummary (Prelude.Maybe Prelude.Text)
templateVersionSummary_description = Lens.lens (\TemplateVersionSummary' {description} -> description) (\s@TemplateVersionSummary' {} a -> s {description = a} :: TemplateVersionSummary)

instance Core.FromJSON TemplateVersionSummary where
  parseJSON =
    Core.withObject
      "TemplateVersionSummary"
      ( \x ->
          TemplateVersionSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "VersionNumber")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable TemplateVersionSummary where
  hashWithSalt salt' TemplateVersionSummary' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status

instance Prelude.NFData TemplateVersionSummary where
  rnf TemplateVersionSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
