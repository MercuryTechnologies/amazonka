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
-- Module      : Amazonka.LicenseManager.Types.ReportGenerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ReportGenerator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types.ReportContext
import Amazonka.LicenseManager.Types.ReportFrequency
import Amazonka.LicenseManager.Types.ReportType
import Amazonka.LicenseManager.Types.S3Location
import Amazonka.LicenseManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describe the details of a report generator.
--
-- /See:/ 'newReportGenerator' smart constructor.
data ReportGenerator = ReportGenerator'
  { -- | Time the last report was generated at.
    lastReportGenerationTime :: Prelude.Maybe Prelude.Text,
    -- | Failure message for the last report generation attempt.
    lastRunFailureReason :: Prelude.Maybe Prelude.Text,
    -- | Status of the last report generation attempt.
    lastRunStatus :: Prelude.Maybe Prelude.Text,
    -- | Name of the report generator.
    reportGeneratorName :: Prelude.Maybe Prelude.Text,
    -- | Details about how frequently reports are generated.
    reportFrequency :: Prelude.Maybe ReportFrequency,
    -- | Details of the S3 bucket that report generator reports are published to.
    s3Location :: Prelude.Maybe S3Location,
    -- | Amazon Resource Name (ARN) of the report generator.
    licenseManagerReportGeneratorArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID used to create the report generator.
    reportCreatorAccount :: Prelude.Maybe Prelude.Text,
    -- | Description of the report generator.
    description :: Prelude.Maybe Prelude.Text,
    -- | Type of reports that are generated.
    reportType :: Prelude.Maybe [ReportType],
    -- | Time the report was created.
    createTime :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the report generator.
    tags :: Prelude.Maybe [Tag],
    -- | License configuration type for this generator.
    reportContext :: Prelude.Maybe ReportContext
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportGenerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastReportGenerationTime', 'reportGenerator_lastReportGenerationTime' - Time the last report was generated at.
--
-- 'lastRunFailureReason', 'reportGenerator_lastRunFailureReason' - Failure message for the last report generation attempt.
--
-- 'lastRunStatus', 'reportGenerator_lastRunStatus' - Status of the last report generation attempt.
--
-- 'reportGeneratorName', 'reportGenerator_reportGeneratorName' - Name of the report generator.
--
-- 'reportFrequency', 'reportGenerator_reportFrequency' - Details about how frequently reports are generated.
--
-- 's3Location', 'reportGenerator_s3Location' - Details of the S3 bucket that report generator reports are published to.
--
-- 'licenseManagerReportGeneratorArn', 'reportGenerator_licenseManagerReportGeneratorArn' - Amazon Resource Name (ARN) of the report generator.
--
-- 'reportCreatorAccount', 'reportGenerator_reportCreatorAccount' - The Amazon Web Services account ID used to create the report generator.
--
-- 'description', 'reportGenerator_description' - Description of the report generator.
--
-- 'reportType', 'reportGenerator_reportType' - Type of reports that are generated.
--
-- 'createTime', 'reportGenerator_createTime' - Time the report was created.
--
-- 'tags', 'reportGenerator_tags' - Tags associated with the report generator.
--
-- 'reportContext', 'reportGenerator_reportContext' - License configuration type for this generator.
newReportGenerator ::
  ReportGenerator
newReportGenerator =
  ReportGenerator'
    { lastReportGenerationTime =
        Prelude.Nothing,
      lastRunFailureReason = Prelude.Nothing,
      lastRunStatus = Prelude.Nothing,
      reportGeneratorName = Prelude.Nothing,
      reportFrequency = Prelude.Nothing,
      s3Location = Prelude.Nothing,
      licenseManagerReportGeneratorArn = Prelude.Nothing,
      reportCreatorAccount = Prelude.Nothing,
      description = Prelude.Nothing,
      reportType = Prelude.Nothing,
      createTime = Prelude.Nothing,
      tags = Prelude.Nothing,
      reportContext = Prelude.Nothing
    }

-- | Time the last report was generated at.
reportGenerator_lastReportGenerationTime :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_lastReportGenerationTime = Lens.lens (\ReportGenerator' {lastReportGenerationTime} -> lastReportGenerationTime) (\s@ReportGenerator' {} a -> s {lastReportGenerationTime = a} :: ReportGenerator)

-- | Failure message for the last report generation attempt.
reportGenerator_lastRunFailureReason :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_lastRunFailureReason = Lens.lens (\ReportGenerator' {lastRunFailureReason} -> lastRunFailureReason) (\s@ReportGenerator' {} a -> s {lastRunFailureReason = a} :: ReportGenerator)

-- | Status of the last report generation attempt.
reportGenerator_lastRunStatus :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_lastRunStatus = Lens.lens (\ReportGenerator' {lastRunStatus} -> lastRunStatus) (\s@ReportGenerator' {} a -> s {lastRunStatus = a} :: ReportGenerator)

-- | Name of the report generator.
reportGenerator_reportGeneratorName :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_reportGeneratorName = Lens.lens (\ReportGenerator' {reportGeneratorName} -> reportGeneratorName) (\s@ReportGenerator' {} a -> s {reportGeneratorName = a} :: ReportGenerator)

-- | Details about how frequently reports are generated.
reportGenerator_reportFrequency :: Lens.Lens' ReportGenerator (Prelude.Maybe ReportFrequency)
reportGenerator_reportFrequency = Lens.lens (\ReportGenerator' {reportFrequency} -> reportFrequency) (\s@ReportGenerator' {} a -> s {reportFrequency = a} :: ReportGenerator)

-- | Details of the S3 bucket that report generator reports are published to.
reportGenerator_s3Location :: Lens.Lens' ReportGenerator (Prelude.Maybe S3Location)
reportGenerator_s3Location = Lens.lens (\ReportGenerator' {s3Location} -> s3Location) (\s@ReportGenerator' {} a -> s {s3Location = a} :: ReportGenerator)

-- | Amazon Resource Name (ARN) of the report generator.
reportGenerator_licenseManagerReportGeneratorArn :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_licenseManagerReportGeneratorArn = Lens.lens (\ReportGenerator' {licenseManagerReportGeneratorArn} -> licenseManagerReportGeneratorArn) (\s@ReportGenerator' {} a -> s {licenseManagerReportGeneratorArn = a} :: ReportGenerator)

-- | The Amazon Web Services account ID used to create the report generator.
reportGenerator_reportCreatorAccount :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_reportCreatorAccount = Lens.lens (\ReportGenerator' {reportCreatorAccount} -> reportCreatorAccount) (\s@ReportGenerator' {} a -> s {reportCreatorAccount = a} :: ReportGenerator)

-- | Description of the report generator.
reportGenerator_description :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_description = Lens.lens (\ReportGenerator' {description} -> description) (\s@ReportGenerator' {} a -> s {description = a} :: ReportGenerator)

-- | Type of reports that are generated.
reportGenerator_reportType :: Lens.Lens' ReportGenerator (Prelude.Maybe [ReportType])
reportGenerator_reportType = Lens.lens (\ReportGenerator' {reportType} -> reportType) (\s@ReportGenerator' {} a -> s {reportType = a} :: ReportGenerator) Prelude.. Lens.mapping Lens.coerced

-- | Time the report was created.
reportGenerator_createTime :: Lens.Lens' ReportGenerator (Prelude.Maybe Prelude.Text)
reportGenerator_createTime = Lens.lens (\ReportGenerator' {createTime} -> createTime) (\s@ReportGenerator' {} a -> s {createTime = a} :: ReportGenerator)

-- | Tags associated with the report generator.
reportGenerator_tags :: Lens.Lens' ReportGenerator (Prelude.Maybe [Tag])
reportGenerator_tags = Lens.lens (\ReportGenerator' {tags} -> tags) (\s@ReportGenerator' {} a -> s {tags = a} :: ReportGenerator) Prelude.. Lens.mapping Lens.coerced

-- | License configuration type for this generator.
reportGenerator_reportContext :: Lens.Lens' ReportGenerator (Prelude.Maybe ReportContext)
reportGenerator_reportContext = Lens.lens (\ReportGenerator' {reportContext} -> reportContext) (\s@ReportGenerator' {} a -> s {reportContext = a} :: ReportGenerator)

instance Core.FromJSON ReportGenerator where
  parseJSON =
    Core.withObject
      "ReportGenerator"
      ( \x ->
          ReportGenerator'
            Prelude.<$> (x Core..:? "LastReportGenerationTime")
            Prelude.<*> (x Core..:? "LastRunFailureReason")
            Prelude.<*> (x Core..:? "LastRunStatus")
            Prelude.<*> (x Core..:? "ReportGeneratorName")
            Prelude.<*> (x Core..:? "ReportFrequency")
            Prelude.<*> (x Core..:? "S3Location")
            Prelude.<*> (x Core..:? "LicenseManagerReportGeneratorArn")
            Prelude.<*> (x Core..:? "ReportCreatorAccount")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ReportType" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CreateTime")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ReportContext")
      )

instance Prelude.Hashable ReportGenerator where
  hashWithSalt salt' ReportGenerator' {..} =
    salt' `Prelude.hashWithSalt` reportContext
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` reportType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` reportCreatorAccount
      `Prelude.hashWithSalt` licenseManagerReportGeneratorArn
      `Prelude.hashWithSalt` s3Location
      `Prelude.hashWithSalt` reportFrequency
      `Prelude.hashWithSalt` reportGeneratorName
      `Prelude.hashWithSalt` lastRunStatus
      `Prelude.hashWithSalt` lastRunFailureReason
      `Prelude.hashWithSalt` lastReportGenerationTime

instance Prelude.NFData ReportGenerator where
  rnf ReportGenerator' {..} =
    Prelude.rnf lastReportGenerationTime
      `Prelude.seq` Prelude.rnf reportContext
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf reportType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf reportCreatorAccount
      `Prelude.seq` Prelude.rnf licenseManagerReportGeneratorArn
      `Prelude.seq` Prelude.rnf s3Location
      `Prelude.seq` Prelude.rnf reportFrequency
      `Prelude.seq` Prelude.rnf reportGeneratorName
      `Prelude.seq` Prelude.rnf lastRunStatus
      `Prelude.seq` Prelude.rnf lastRunFailureReason
