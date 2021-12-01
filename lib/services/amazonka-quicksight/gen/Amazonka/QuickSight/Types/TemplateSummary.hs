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
-- Module      : Amazonka.QuickSight.Types.TemplateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The template summary.
--
-- /See:/ 'newTemplateSummary' smart constructor.
data TemplateSummary = TemplateSummary'
  { -- | The last time that this template was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | A structure containing a list of version numbers for the template
    -- summary.
    latestVersionNumber :: Prelude.Maybe Prelude.Natural,
    -- | A summary of a template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The last time that this template was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the template. This ID is unique per Amazon Web Services Region
    -- for each Amazon Web Services account.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | A display name for the template.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'templateSummary_lastUpdatedTime' - The last time that this template was updated.
--
-- 'latestVersionNumber', 'templateSummary_latestVersionNumber' - A structure containing a list of version numbers for the template
-- summary.
--
-- 'arn', 'templateSummary_arn' - A summary of a template.
--
-- 'createdTime', 'templateSummary_createdTime' - The last time that this template was created.
--
-- 'templateId', 'templateSummary_templateId' - The ID of the template. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
--
-- 'name', 'templateSummary_name' - A display name for the template.
newTemplateSummary ::
  TemplateSummary
newTemplateSummary =
  TemplateSummary'
    { lastUpdatedTime = Prelude.Nothing,
      latestVersionNumber = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      templateId = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The last time that this template was updated.
templateSummary_lastUpdatedTime :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.UTCTime)
templateSummary_lastUpdatedTime = Lens.lens (\TemplateSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@TemplateSummary' {} a -> s {lastUpdatedTime = a} :: TemplateSummary) Prelude.. Lens.mapping Core._Time

-- | A structure containing a list of version numbers for the template
-- summary.
templateSummary_latestVersionNumber :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Natural)
templateSummary_latestVersionNumber = Lens.lens (\TemplateSummary' {latestVersionNumber} -> latestVersionNumber) (\s@TemplateSummary' {} a -> s {latestVersionNumber = a} :: TemplateSummary)

-- | A summary of a template.
templateSummary_arn :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_arn = Lens.lens (\TemplateSummary' {arn} -> arn) (\s@TemplateSummary' {} a -> s {arn = a} :: TemplateSummary)

-- | The last time that this template was created.
templateSummary_createdTime :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.UTCTime)
templateSummary_createdTime = Lens.lens (\TemplateSummary' {createdTime} -> createdTime) (\s@TemplateSummary' {} a -> s {createdTime = a} :: TemplateSummary) Prelude.. Lens.mapping Core._Time

-- | The ID of the template. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
templateSummary_templateId :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_templateId = Lens.lens (\TemplateSummary' {templateId} -> templateId) (\s@TemplateSummary' {} a -> s {templateId = a} :: TemplateSummary)

-- | A display name for the template.
templateSummary_name :: Lens.Lens' TemplateSummary (Prelude.Maybe Prelude.Text)
templateSummary_name = Lens.lens (\TemplateSummary' {name} -> name) (\s@TemplateSummary' {} a -> s {name = a} :: TemplateSummary)

instance Core.FromJSON TemplateSummary where
  parseJSON =
    Core.withObject
      "TemplateSummary"
      ( \x ->
          TemplateSummary'
            Prelude.<$> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "LatestVersionNumber")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "TemplateId")
            Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable TemplateSummary where
  hashWithSalt salt' TemplateSummary' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` latestVersionNumber
      `Prelude.hashWithSalt` lastUpdatedTime

instance Prelude.NFData TemplateSummary where
  rnf TemplateSummary' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf latestVersionNumber
