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
-- Module      : Amazonka.PinpointEmail.Types.DeliverabilityTestReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.DeliverabilityTestReport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.PinpointEmail.Types.DeliverabilityTestStatus
import qualified Amazonka.Prelude as Prelude

-- | An object that contains metadata related to a predictive inbox placement
-- test.
--
-- /See:/ 'newDeliverabilityTestReport' smart constructor.
data DeliverabilityTestReport = DeliverabilityTestReport'
  { -- | The subject line for an email that you submitted in a predictive inbox
    -- placement test.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The sender address that you specified for the predictive inbox placement
    -- test.
    fromEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the predictive inbox placement test was created,
    -- in Unix time format.
    createDate :: Prelude.Maybe Core.POSIX,
    -- | A unique string that identifies the predictive inbox placement test.
    reportId :: Prelude.Maybe Prelude.Text,
    -- | A name that helps you identify a predictive inbox placement test report.
    reportName :: Prelude.Maybe Prelude.Text,
    -- | The status of the predictive inbox placement test. If the status is
    -- @IN_PROGRESS@, then the predictive inbox placement test is currently
    -- running. Predictive inbox placement tests are usually complete within 24
    -- hours of creating the test. If the status is @COMPLETE@, then the test
    -- is finished, and you can use the @GetDeliverabilityTestReport@ to view
    -- the results of the test.
    deliverabilityTestStatus :: Prelude.Maybe DeliverabilityTestStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeliverabilityTestReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subject', 'deliverabilityTestReport_subject' - The subject line for an email that you submitted in a predictive inbox
-- placement test.
--
-- 'fromEmailAddress', 'deliverabilityTestReport_fromEmailAddress' - The sender address that you specified for the predictive inbox placement
-- test.
--
-- 'createDate', 'deliverabilityTestReport_createDate' - The date and time when the predictive inbox placement test was created,
-- in Unix time format.
--
-- 'reportId', 'deliverabilityTestReport_reportId' - A unique string that identifies the predictive inbox placement test.
--
-- 'reportName', 'deliverabilityTestReport_reportName' - A name that helps you identify a predictive inbox placement test report.
--
-- 'deliverabilityTestStatus', 'deliverabilityTestReport_deliverabilityTestStatus' - The status of the predictive inbox placement test. If the status is
-- @IN_PROGRESS@, then the predictive inbox placement test is currently
-- running. Predictive inbox placement tests are usually complete within 24
-- hours of creating the test. If the status is @COMPLETE@, then the test
-- is finished, and you can use the @GetDeliverabilityTestReport@ to view
-- the results of the test.
newDeliverabilityTestReport ::
  DeliverabilityTestReport
newDeliverabilityTestReport =
  DeliverabilityTestReport'
    { subject =
        Prelude.Nothing,
      fromEmailAddress = Prelude.Nothing,
      createDate = Prelude.Nothing,
      reportId = Prelude.Nothing,
      reportName = Prelude.Nothing,
      deliverabilityTestStatus = Prelude.Nothing
    }

-- | The subject line for an email that you submitted in a predictive inbox
-- placement test.
deliverabilityTestReport_subject :: Lens.Lens' DeliverabilityTestReport (Prelude.Maybe Prelude.Text)
deliverabilityTestReport_subject = Lens.lens (\DeliverabilityTestReport' {subject} -> subject) (\s@DeliverabilityTestReport' {} a -> s {subject = a} :: DeliverabilityTestReport)

-- | The sender address that you specified for the predictive inbox placement
-- test.
deliverabilityTestReport_fromEmailAddress :: Lens.Lens' DeliverabilityTestReport (Prelude.Maybe Prelude.Text)
deliverabilityTestReport_fromEmailAddress = Lens.lens (\DeliverabilityTestReport' {fromEmailAddress} -> fromEmailAddress) (\s@DeliverabilityTestReport' {} a -> s {fromEmailAddress = a} :: DeliverabilityTestReport)

-- | The date and time when the predictive inbox placement test was created,
-- in Unix time format.
deliverabilityTestReport_createDate :: Lens.Lens' DeliverabilityTestReport (Prelude.Maybe Prelude.UTCTime)
deliverabilityTestReport_createDate = Lens.lens (\DeliverabilityTestReport' {createDate} -> createDate) (\s@DeliverabilityTestReport' {} a -> s {createDate = a} :: DeliverabilityTestReport) Prelude.. Lens.mapping Core._Time

-- | A unique string that identifies the predictive inbox placement test.
deliverabilityTestReport_reportId :: Lens.Lens' DeliverabilityTestReport (Prelude.Maybe Prelude.Text)
deliverabilityTestReport_reportId = Lens.lens (\DeliverabilityTestReport' {reportId} -> reportId) (\s@DeliverabilityTestReport' {} a -> s {reportId = a} :: DeliverabilityTestReport)

-- | A name that helps you identify a predictive inbox placement test report.
deliverabilityTestReport_reportName :: Lens.Lens' DeliverabilityTestReport (Prelude.Maybe Prelude.Text)
deliverabilityTestReport_reportName = Lens.lens (\DeliverabilityTestReport' {reportName} -> reportName) (\s@DeliverabilityTestReport' {} a -> s {reportName = a} :: DeliverabilityTestReport)

-- | The status of the predictive inbox placement test. If the status is
-- @IN_PROGRESS@, then the predictive inbox placement test is currently
-- running. Predictive inbox placement tests are usually complete within 24
-- hours of creating the test. If the status is @COMPLETE@, then the test
-- is finished, and you can use the @GetDeliverabilityTestReport@ to view
-- the results of the test.
deliverabilityTestReport_deliverabilityTestStatus :: Lens.Lens' DeliverabilityTestReport (Prelude.Maybe DeliverabilityTestStatus)
deliverabilityTestReport_deliverabilityTestStatus = Lens.lens (\DeliverabilityTestReport' {deliverabilityTestStatus} -> deliverabilityTestStatus) (\s@DeliverabilityTestReport' {} a -> s {deliverabilityTestStatus = a} :: DeliverabilityTestReport)

instance Core.FromJSON DeliverabilityTestReport where
  parseJSON =
    Core.withObject
      "DeliverabilityTestReport"
      ( \x ->
          DeliverabilityTestReport'
            Prelude.<$> (x Core..:? "Subject")
            Prelude.<*> (x Core..:? "FromEmailAddress")
            Prelude.<*> (x Core..:? "CreateDate")
            Prelude.<*> (x Core..:? "ReportId")
            Prelude.<*> (x Core..:? "ReportName")
            Prelude.<*> (x Core..:? "DeliverabilityTestStatus")
      )

instance Prelude.Hashable DeliverabilityTestReport where
  hashWithSalt salt' DeliverabilityTestReport' {..} =
    salt'
      `Prelude.hashWithSalt` deliverabilityTestStatus
      `Prelude.hashWithSalt` reportName
      `Prelude.hashWithSalt` reportId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` fromEmailAddress
      `Prelude.hashWithSalt` subject

instance Prelude.NFData DeliverabilityTestReport where
  rnf DeliverabilityTestReport' {..} =
    Prelude.rnf subject
      `Prelude.seq` Prelude.rnf deliverabilityTestStatus
      `Prelude.seq` Prelude.rnf reportName
      `Prelude.seq` Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf fromEmailAddress
