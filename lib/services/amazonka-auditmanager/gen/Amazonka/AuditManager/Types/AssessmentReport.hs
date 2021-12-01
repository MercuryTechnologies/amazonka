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
-- Module      : Amazonka.AuditManager.Types.AssessmentReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.AssessmentReport where

import Amazonka.AuditManager.Types.AssessmentReportStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A finalized document generated from an Audit Manager assessment. These
-- reports summarize the relevant evidence collected for your audit, and
-- link to the relevant evidence folders which are named and organized
-- according to the controls specified in your assessment.
--
-- /See:/ 'newAssessmentReport' smart constructor.
data AssessmentReport = AssessmentReport'
  { -- | Specifies when the assessment report was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The current status of the specified assessment report.
    status :: Prelude.Maybe AssessmentReportStatus,
    -- | The identifier for the specified Amazon Web Services account.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The name given to the assessment report.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the user who created the assessment report.
    author :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the specified assessment report.
    id :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified assessment.
    assessmentId :: Prelude.Maybe Prelude.Text,
    -- | The description of the specified assessment report.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the associated assessment.
    assessmentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssessmentReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'assessmentReport_creationTime' - Specifies when the assessment report was created.
--
-- 'status', 'assessmentReport_status' - The current status of the specified assessment report.
--
-- 'awsAccountId', 'assessmentReport_awsAccountId' - The identifier for the specified Amazon Web Services account.
--
-- 'name', 'assessmentReport_name' - The name given to the assessment report.
--
-- 'author', 'assessmentReport_author' - The name of the user who created the assessment report.
--
-- 'id', 'assessmentReport_id' - The unique identifier for the specified assessment report.
--
-- 'assessmentId', 'assessmentReport_assessmentId' - The identifier for the specified assessment.
--
-- 'description', 'assessmentReport_description' - The description of the specified assessment report.
--
-- 'assessmentName', 'assessmentReport_assessmentName' - The name of the associated assessment.
newAssessmentReport ::
  AssessmentReport
newAssessmentReport =
  AssessmentReport'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      name = Prelude.Nothing,
      author = Prelude.Nothing,
      id = Prelude.Nothing,
      assessmentId = Prelude.Nothing,
      description = Prelude.Nothing,
      assessmentName = Prelude.Nothing
    }

-- | Specifies when the assessment report was created.
assessmentReport_creationTime :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.UTCTime)
assessmentReport_creationTime = Lens.lens (\AssessmentReport' {creationTime} -> creationTime) (\s@AssessmentReport' {} a -> s {creationTime = a} :: AssessmentReport) Prelude.. Lens.mapping Core._Time

-- | The current status of the specified assessment report.
assessmentReport_status :: Lens.Lens' AssessmentReport (Prelude.Maybe AssessmentReportStatus)
assessmentReport_status = Lens.lens (\AssessmentReport' {status} -> status) (\s@AssessmentReport' {} a -> s {status = a} :: AssessmentReport)

-- | The identifier for the specified Amazon Web Services account.
assessmentReport_awsAccountId :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_awsAccountId = Lens.lens (\AssessmentReport' {awsAccountId} -> awsAccountId) (\s@AssessmentReport' {} a -> s {awsAccountId = a} :: AssessmentReport)

-- | The name given to the assessment report.
assessmentReport_name :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_name = Lens.lens (\AssessmentReport' {name} -> name) (\s@AssessmentReport' {} a -> s {name = a} :: AssessmentReport)

-- | The name of the user who created the assessment report.
assessmentReport_author :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_author = Lens.lens (\AssessmentReport' {author} -> author) (\s@AssessmentReport' {} a -> s {author = a} :: AssessmentReport)

-- | The unique identifier for the specified assessment report.
assessmentReport_id :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_id = Lens.lens (\AssessmentReport' {id} -> id) (\s@AssessmentReport' {} a -> s {id = a} :: AssessmentReport)

-- | The identifier for the specified assessment.
assessmentReport_assessmentId :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_assessmentId = Lens.lens (\AssessmentReport' {assessmentId} -> assessmentId) (\s@AssessmentReport' {} a -> s {assessmentId = a} :: AssessmentReport)

-- | The description of the specified assessment report.
assessmentReport_description :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_description = Lens.lens (\AssessmentReport' {description} -> description) (\s@AssessmentReport' {} a -> s {description = a} :: AssessmentReport)

-- | The name of the associated assessment.
assessmentReport_assessmentName :: Lens.Lens' AssessmentReport (Prelude.Maybe Prelude.Text)
assessmentReport_assessmentName = Lens.lens (\AssessmentReport' {assessmentName} -> assessmentName) (\s@AssessmentReport' {} a -> s {assessmentName = a} :: AssessmentReport)

instance Core.FromJSON AssessmentReport where
  parseJSON =
    Core.withObject
      "AssessmentReport"
      ( \x ->
          AssessmentReport'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "awsAccountId")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "author")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "assessmentId")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "assessmentName")
      )

instance Prelude.Hashable AssessmentReport where
  hashWithSalt salt' AssessmentReport' {..} =
    salt' `Prelude.hashWithSalt` assessmentName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` assessmentId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData AssessmentReport where
  rnf AssessmentReport' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf assessmentName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf assessmentId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf status
