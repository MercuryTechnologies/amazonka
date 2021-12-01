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
-- Module      : Amazonka.IoT.Types.AuditFinding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuditFinding where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.AuditFindingSeverity
import Amazonka.IoT.Types.NonCompliantResource
import Amazonka.IoT.Types.RelatedResource
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The findings (results) of the audit.
--
-- /See:/ 'newAuditFinding' smart constructor.
data AuditFinding = AuditFinding'
  { -- | Indicates whether the audit finding was suppressed or not during
    -- reporting.
    isSuppressed :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the audit that generated this result (finding).
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The time the result (finding) was discovered.
    findingTime :: Prelude.Maybe Core.POSIX,
    -- | The time the audit started.
    taskStartTime :: Prelude.Maybe Core.POSIX,
    -- | A code that indicates the reason that the resource was noncompliant.
    reasonForNonComplianceCode :: Prelude.Maybe Prelude.Text,
    -- | The severity of the result (finding).
    severity :: Prelude.Maybe AuditFindingSeverity,
    -- | The list of related resources.
    relatedResources :: Prelude.Maybe [RelatedResource],
    -- | The audit check that generated this result.
    checkName :: Prelude.Maybe Prelude.Text,
    -- | The resource that was found to be noncompliant with the audit check.
    nonCompliantResource :: Prelude.Maybe NonCompliantResource,
    -- | The reason the resource was noncompliant.
    reasonForNonCompliance :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for this set of audit findings. This identifier is
    -- used to apply mitigation tasks to one or more sets of findings.
    findingId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuditFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isSuppressed', 'auditFinding_isSuppressed' - Indicates whether the audit finding was suppressed or not during
-- reporting.
--
-- 'taskId', 'auditFinding_taskId' - The ID of the audit that generated this result (finding).
--
-- 'findingTime', 'auditFinding_findingTime' - The time the result (finding) was discovered.
--
-- 'taskStartTime', 'auditFinding_taskStartTime' - The time the audit started.
--
-- 'reasonForNonComplianceCode', 'auditFinding_reasonForNonComplianceCode' - A code that indicates the reason that the resource was noncompliant.
--
-- 'severity', 'auditFinding_severity' - The severity of the result (finding).
--
-- 'relatedResources', 'auditFinding_relatedResources' - The list of related resources.
--
-- 'checkName', 'auditFinding_checkName' - The audit check that generated this result.
--
-- 'nonCompliantResource', 'auditFinding_nonCompliantResource' - The resource that was found to be noncompliant with the audit check.
--
-- 'reasonForNonCompliance', 'auditFinding_reasonForNonCompliance' - The reason the resource was noncompliant.
--
-- 'findingId', 'auditFinding_findingId' - A unique identifier for this set of audit findings. This identifier is
-- used to apply mitigation tasks to one or more sets of findings.
newAuditFinding ::
  AuditFinding
newAuditFinding =
  AuditFinding'
    { isSuppressed = Prelude.Nothing,
      taskId = Prelude.Nothing,
      findingTime = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      reasonForNonComplianceCode = Prelude.Nothing,
      severity = Prelude.Nothing,
      relatedResources = Prelude.Nothing,
      checkName = Prelude.Nothing,
      nonCompliantResource = Prelude.Nothing,
      reasonForNonCompliance = Prelude.Nothing,
      findingId = Prelude.Nothing
    }

-- | Indicates whether the audit finding was suppressed or not during
-- reporting.
auditFinding_isSuppressed :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Bool)
auditFinding_isSuppressed = Lens.lens (\AuditFinding' {isSuppressed} -> isSuppressed) (\s@AuditFinding' {} a -> s {isSuppressed = a} :: AuditFinding)

-- | The ID of the audit that generated this result (finding).
auditFinding_taskId :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_taskId = Lens.lens (\AuditFinding' {taskId} -> taskId) (\s@AuditFinding' {} a -> s {taskId = a} :: AuditFinding)

-- | The time the result (finding) was discovered.
auditFinding_findingTime :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.UTCTime)
auditFinding_findingTime = Lens.lens (\AuditFinding' {findingTime} -> findingTime) (\s@AuditFinding' {} a -> s {findingTime = a} :: AuditFinding) Prelude.. Lens.mapping Core._Time

-- | The time the audit started.
auditFinding_taskStartTime :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.UTCTime)
auditFinding_taskStartTime = Lens.lens (\AuditFinding' {taskStartTime} -> taskStartTime) (\s@AuditFinding' {} a -> s {taskStartTime = a} :: AuditFinding) Prelude.. Lens.mapping Core._Time

-- | A code that indicates the reason that the resource was noncompliant.
auditFinding_reasonForNonComplianceCode :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_reasonForNonComplianceCode = Lens.lens (\AuditFinding' {reasonForNonComplianceCode} -> reasonForNonComplianceCode) (\s@AuditFinding' {} a -> s {reasonForNonComplianceCode = a} :: AuditFinding)

-- | The severity of the result (finding).
auditFinding_severity :: Lens.Lens' AuditFinding (Prelude.Maybe AuditFindingSeverity)
auditFinding_severity = Lens.lens (\AuditFinding' {severity} -> severity) (\s@AuditFinding' {} a -> s {severity = a} :: AuditFinding)

-- | The list of related resources.
auditFinding_relatedResources :: Lens.Lens' AuditFinding (Prelude.Maybe [RelatedResource])
auditFinding_relatedResources = Lens.lens (\AuditFinding' {relatedResources} -> relatedResources) (\s@AuditFinding' {} a -> s {relatedResources = a} :: AuditFinding) Prelude.. Lens.mapping Lens.coerced

-- | The audit check that generated this result.
auditFinding_checkName :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_checkName = Lens.lens (\AuditFinding' {checkName} -> checkName) (\s@AuditFinding' {} a -> s {checkName = a} :: AuditFinding)

-- | The resource that was found to be noncompliant with the audit check.
auditFinding_nonCompliantResource :: Lens.Lens' AuditFinding (Prelude.Maybe NonCompliantResource)
auditFinding_nonCompliantResource = Lens.lens (\AuditFinding' {nonCompliantResource} -> nonCompliantResource) (\s@AuditFinding' {} a -> s {nonCompliantResource = a} :: AuditFinding)

-- | The reason the resource was noncompliant.
auditFinding_reasonForNonCompliance :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_reasonForNonCompliance = Lens.lens (\AuditFinding' {reasonForNonCompliance} -> reasonForNonCompliance) (\s@AuditFinding' {} a -> s {reasonForNonCompliance = a} :: AuditFinding)

-- | A unique identifier for this set of audit findings. This identifier is
-- used to apply mitigation tasks to one or more sets of findings.
auditFinding_findingId :: Lens.Lens' AuditFinding (Prelude.Maybe Prelude.Text)
auditFinding_findingId = Lens.lens (\AuditFinding' {findingId} -> findingId) (\s@AuditFinding' {} a -> s {findingId = a} :: AuditFinding)

instance Core.FromJSON AuditFinding where
  parseJSON =
    Core.withObject
      "AuditFinding"
      ( \x ->
          AuditFinding'
            Prelude.<$> (x Core..:? "isSuppressed")
            Prelude.<*> (x Core..:? "taskId")
            Prelude.<*> (x Core..:? "findingTime")
            Prelude.<*> (x Core..:? "taskStartTime")
            Prelude.<*> (x Core..:? "reasonForNonComplianceCode")
            Prelude.<*> (x Core..:? "severity")
            Prelude.<*> ( x Core..:? "relatedResources"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "checkName")
            Prelude.<*> (x Core..:? "nonCompliantResource")
            Prelude.<*> (x Core..:? "reasonForNonCompliance")
            Prelude.<*> (x Core..:? "findingId")
      )

instance Prelude.Hashable AuditFinding where
  hashWithSalt salt' AuditFinding' {..} =
    salt' `Prelude.hashWithSalt` findingId
      `Prelude.hashWithSalt` reasonForNonCompliance
      `Prelude.hashWithSalt` nonCompliantResource
      `Prelude.hashWithSalt` checkName
      `Prelude.hashWithSalt` relatedResources
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` reasonForNonComplianceCode
      `Prelude.hashWithSalt` taskStartTime
      `Prelude.hashWithSalt` findingTime
      `Prelude.hashWithSalt` taskId
      `Prelude.hashWithSalt` isSuppressed

instance Prelude.NFData AuditFinding where
  rnf AuditFinding' {..} =
    Prelude.rnf isSuppressed
      `Prelude.seq` Prelude.rnf findingId
      `Prelude.seq` Prelude.rnf reasonForNonCompliance
      `Prelude.seq` Prelude.rnf nonCompliantResource
      `Prelude.seq` Prelude.rnf checkName
      `Prelude.seq` Prelude.rnf relatedResources
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf reasonForNonComplianceCode
      `Prelude.seq` Prelude.rnf taskStartTime
      `Prelude.seq` Prelude.rnf findingTime
      `Prelude.seq` Prelude.rnf taskId
