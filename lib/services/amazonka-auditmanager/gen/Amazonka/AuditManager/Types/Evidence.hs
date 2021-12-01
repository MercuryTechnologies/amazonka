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
-- Module      : Amazonka.AuditManager.Types.Evidence
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Evidence where

import Amazonka.AuditManager.Types.Resource
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A record that contains the information needed to demonstrate compliance
-- with the requirements specified by a control. Examples of evidence
-- include change activity triggered by a user, or a system configuration
-- snapshot.
--
-- /See:/ 'newEvidence' smart constructor.
data Evidence = Evidence'
  { -- | The timestamp that represents when the evidence was collected.
    time :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether the evidence is included in the assessment report.
    assessmentReportSelection :: Prelude.Maybe Prelude.Text,
    -- | The type of automated evidence.
    evidenceByType :: Prelude.Maybe Prelude.Text,
    -- | The evaluation status for evidence that falls under the compliance check
    -- category. For evidence collected from Security Hub, a /Pass/ or /Fail/
    -- result is shown. For evidence collected from Config, a /Compliant/ or
    -- /Noncompliant/ result is shown.
    complianceCheck :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account from which the evidence is collected,
    -- and its organization path.
    awsOrganization :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the specified Amazon Web Services account.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The names and values used by the evidence event, including an attribute
    -- name (such as @allowUsersToChangePassword@) and value (such as @true@ or
    -- @false@).
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier for the specified Amazon Web Services account.
    evidenceAwsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the evidence.
    id :: Prelude.Maybe Prelude.Text,
    -- | The data source from which the specified evidence was collected.
    dataSource :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the folder in which the evidence is stored.
    evidenceFolderId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the IAM user or role associated with the
    -- evidence.
    iamId :: Prelude.Maybe Prelude.Text,
    -- | The name of the specified evidence event.
    eventName :: Prelude.Maybe Prelude.Text,
    -- | The list of resources assessed to generate the evidence.
    resourcesIncluded :: Prelude.Maybe [Resource],
    -- | The Amazon Web Service from which the evidence is collected.
    eventSource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Evidence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'time', 'evidence_time' - The timestamp that represents when the evidence was collected.
--
-- 'assessmentReportSelection', 'evidence_assessmentReportSelection' - Specifies whether the evidence is included in the assessment report.
--
-- 'evidenceByType', 'evidence_evidenceByType' - The type of automated evidence.
--
-- 'complianceCheck', 'evidence_complianceCheck' - The evaluation status for evidence that falls under the compliance check
-- category. For evidence collected from Security Hub, a /Pass/ or /Fail/
-- result is shown. For evidence collected from Config, a /Compliant/ or
-- /Noncompliant/ result is shown.
--
-- 'awsOrganization', 'evidence_awsOrganization' - The Amazon Web Services account from which the evidence is collected,
-- and its organization path.
--
-- 'awsAccountId', 'evidence_awsAccountId' - The identifier for the specified Amazon Web Services account.
--
-- 'attributes', 'evidence_attributes' - The names and values used by the evidence event, including an attribute
-- name (such as @allowUsersToChangePassword@) and value (such as @true@ or
-- @false@).
--
-- 'evidenceAwsAccountId', 'evidence_evidenceAwsAccountId' - The identifier for the specified Amazon Web Services account.
--
-- 'id', 'evidence_id' - The identifier for the evidence.
--
-- 'dataSource', 'evidence_dataSource' - The data source from which the specified evidence was collected.
--
-- 'evidenceFolderId', 'evidence_evidenceFolderId' - The identifier for the folder in which the evidence is stored.
--
-- 'iamId', 'evidence_iamId' - The unique identifier for the IAM user or role associated with the
-- evidence.
--
-- 'eventName', 'evidence_eventName' - The name of the specified evidence event.
--
-- 'resourcesIncluded', 'evidence_resourcesIncluded' - The list of resources assessed to generate the evidence.
--
-- 'eventSource', 'evidence_eventSource' - The Amazon Web Service from which the evidence is collected.
newEvidence ::
  Evidence
newEvidence =
  Evidence'
    { time = Prelude.Nothing,
      assessmentReportSelection = Prelude.Nothing,
      evidenceByType = Prelude.Nothing,
      complianceCheck = Prelude.Nothing,
      awsOrganization = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      attributes = Prelude.Nothing,
      evidenceAwsAccountId = Prelude.Nothing,
      id = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      evidenceFolderId = Prelude.Nothing,
      iamId = Prelude.Nothing,
      eventName = Prelude.Nothing,
      resourcesIncluded = Prelude.Nothing,
      eventSource = Prelude.Nothing
    }

-- | The timestamp that represents when the evidence was collected.
evidence_time :: Lens.Lens' Evidence (Prelude.Maybe Prelude.UTCTime)
evidence_time = Lens.lens (\Evidence' {time} -> time) (\s@Evidence' {} a -> s {time = a} :: Evidence) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the evidence is included in the assessment report.
evidence_assessmentReportSelection :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_assessmentReportSelection = Lens.lens (\Evidence' {assessmentReportSelection} -> assessmentReportSelection) (\s@Evidence' {} a -> s {assessmentReportSelection = a} :: Evidence)

-- | The type of automated evidence.
evidence_evidenceByType :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_evidenceByType = Lens.lens (\Evidence' {evidenceByType} -> evidenceByType) (\s@Evidence' {} a -> s {evidenceByType = a} :: Evidence)

-- | The evaluation status for evidence that falls under the compliance check
-- category. For evidence collected from Security Hub, a /Pass/ or /Fail/
-- result is shown. For evidence collected from Config, a /Compliant/ or
-- /Noncompliant/ result is shown.
evidence_complianceCheck :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_complianceCheck = Lens.lens (\Evidence' {complianceCheck} -> complianceCheck) (\s@Evidence' {} a -> s {complianceCheck = a} :: Evidence)

-- | The Amazon Web Services account from which the evidence is collected,
-- and its organization path.
evidence_awsOrganization :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_awsOrganization = Lens.lens (\Evidence' {awsOrganization} -> awsOrganization) (\s@Evidence' {} a -> s {awsOrganization = a} :: Evidence)

-- | The identifier for the specified Amazon Web Services account.
evidence_awsAccountId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_awsAccountId = Lens.lens (\Evidence' {awsAccountId} -> awsAccountId) (\s@Evidence' {} a -> s {awsAccountId = a} :: Evidence)

-- | The names and values used by the evidence event, including an attribute
-- name (such as @allowUsersToChangePassword@) and value (such as @true@ or
-- @false@).
evidence_attributes :: Lens.Lens' Evidence (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
evidence_attributes = Lens.lens (\Evidence' {attributes} -> attributes) (\s@Evidence' {} a -> s {attributes = a} :: Evidence) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the specified Amazon Web Services account.
evidence_evidenceAwsAccountId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_evidenceAwsAccountId = Lens.lens (\Evidence' {evidenceAwsAccountId} -> evidenceAwsAccountId) (\s@Evidence' {} a -> s {evidenceAwsAccountId = a} :: Evidence)

-- | The identifier for the evidence.
evidence_id :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_id = Lens.lens (\Evidence' {id} -> id) (\s@Evidence' {} a -> s {id = a} :: Evidence)

-- | The data source from which the specified evidence was collected.
evidence_dataSource :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_dataSource = Lens.lens (\Evidence' {dataSource} -> dataSource) (\s@Evidence' {} a -> s {dataSource = a} :: Evidence)

-- | The identifier for the folder in which the evidence is stored.
evidence_evidenceFolderId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_evidenceFolderId = Lens.lens (\Evidence' {evidenceFolderId} -> evidenceFolderId) (\s@Evidence' {} a -> s {evidenceFolderId = a} :: Evidence)

-- | The unique identifier for the IAM user or role associated with the
-- evidence.
evidence_iamId :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_iamId = Lens.lens (\Evidence' {iamId} -> iamId) (\s@Evidence' {} a -> s {iamId = a} :: Evidence)

-- | The name of the specified evidence event.
evidence_eventName :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_eventName = Lens.lens (\Evidence' {eventName} -> eventName) (\s@Evidence' {} a -> s {eventName = a} :: Evidence)

-- | The list of resources assessed to generate the evidence.
evidence_resourcesIncluded :: Lens.Lens' Evidence (Prelude.Maybe [Resource])
evidence_resourcesIncluded = Lens.lens (\Evidence' {resourcesIncluded} -> resourcesIncluded) (\s@Evidence' {} a -> s {resourcesIncluded = a} :: Evidence) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Service from which the evidence is collected.
evidence_eventSource :: Lens.Lens' Evidence (Prelude.Maybe Prelude.Text)
evidence_eventSource = Lens.lens (\Evidence' {eventSource} -> eventSource) (\s@Evidence' {} a -> s {eventSource = a} :: Evidence)

instance Core.FromJSON Evidence where
  parseJSON =
    Core.withObject
      "Evidence"
      ( \x ->
          Evidence'
            Prelude.<$> (x Core..:? "time")
            Prelude.<*> (x Core..:? "assessmentReportSelection")
            Prelude.<*> (x Core..:? "evidenceByType")
            Prelude.<*> (x Core..:? "complianceCheck")
            Prelude.<*> (x Core..:? "awsOrganization")
            Prelude.<*> (x Core..:? "awsAccountId")
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "evidenceAwsAccountId")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "dataSource")
            Prelude.<*> (x Core..:? "evidenceFolderId")
            Prelude.<*> (x Core..:? "iamId")
            Prelude.<*> (x Core..:? "eventName")
            Prelude.<*> ( x Core..:? "resourcesIncluded"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "eventSource")
      )

instance Prelude.Hashable Evidence where
  hashWithSalt salt' Evidence' {..} =
    salt' `Prelude.hashWithSalt` eventSource
      `Prelude.hashWithSalt` resourcesIncluded
      `Prelude.hashWithSalt` eventName
      `Prelude.hashWithSalt` iamId
      `Prelude.hashWithSalt` evidenceFolderId
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` evidenceAwsAccountId
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` awsOrganization
      `Prelude.hashWithSalt` complianceCheck
      `Prelude.hashWithSalt` evidenceByType
      `Prelude.hashWithSalt` assessmentReportSelection
      `Prelude.hashWithSalt` time

instance Prelude.NFData Evidence where
  rnf Evidence' {..} =
    Prelude.rnf time
      `Prelude.seq` Prelude.rnf eventSource
      `Prelude.seq` Prelude.rnf resourcesIncluded
      `Prelude.seq` Prelude.rnf eventName
      `Prelude.seq` Prelude.rnf iamId
      `Prelude.seq` Prelude.rnf evidenceFolderId
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf evidenceAwsAccountId
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf awsOrganization
      `Prelude.seq` Prelude.rnf complianceCheck
      `Prelude.seq` Prelude.rnf evidenceByType
      `Prelude.seq` Prelude.rnf assessmentReportSelection
