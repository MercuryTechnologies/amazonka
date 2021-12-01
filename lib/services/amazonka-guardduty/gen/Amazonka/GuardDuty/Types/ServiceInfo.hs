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
-- Module      : Amazonka.GuardDuty.Types.ServiceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ServiceInfo where

import qualified Amazonka.Core as Core
import Amazonka.GuardDuty.Types.Action
import Amazonka.GuardDuty.Types.Evidence
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains additional information about the generated finding.
--
-- /See:/ 'newServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { -- | The total count of the occurrences of this finding type.
    count :: Prelude.Maybe Prelude.Int,
    -- | The first-seen timestamp of the activity that prompted GuardDuty to
    -- generate this finding.
    eventFirstSeen :: Prelude.Maybe Prelude.Text,
    -- | Information about the activity that is described in a finding.
    action :: Prelude.Maybe Action,
    -- | The detector ID for the GuardDuty service.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS service (GuardDuty) that generated a finding.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | Feedback that was submitted about the finding.
    userFeedback :: Prelude.Maybe Prelude.Text,
    -- | An evidence object associated with the service.
    evidence :: Prelude.Maybe Evidence,
    -- | The last-seen timestamp of the activity that prompted GuardDuty to
    -- generate this finding.
    eventLastSeen :: Prelude.Maybe Prelude.Text,
    -- | The resource role information for this finding.
    resourceRole :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this finding is archived.
    archived :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'serviceInfo_count' - The total count of the occurrences of this finding type.
--
-- 'eventFirstSeen', 'serviceInfo_eventFirstSeen' - The first-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
--
-- 'action', 'serviceInfo_action' - Information about the activity that is described in a finding.
--
-- 'detectorId', 'serviceInfo_detectorId' - The detector ID for the GuardDuty service.
--
-- 'serviceName', 'serviceInfo_serviceName' - The name of the AWS service (GuardDuty) that generated a finding.
--
-- 'userFeedback', 'serviceInfo_userFeedback' - Feedback that was submitted about the finding.
--
-- 'evidence', 'serviceInfo_evidence' - An evidence object associated with the service.
--
-- 'eventLastSeen', 'serviceInfo_eventLastSeen' - The last-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
--
-- 'resourceRole', 'serviceInfo_resourceRole' - The resource role information for this finding.
--
-- 'archived', 'serviceInfo_archived' - Indicates whether this finding is archived.
newServiceInfo ::
  ServiceInfo
newServiceInfo =
  ServiceInfo'
    { count = Prelude.Nothing,
      eventFirstSeen = Prelude.Nothing,
      action = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      userFeedback = Prelude.Nothing,
      evidence = Prelude.Nothing,
      eventLastSeen = Prelude.Nothing,
      resourceRole = Prelude.Nothing,
      archived = Prelude.Nothing
    }

-- | The total count of the occurrences of this finding type.
serviceInfo_count :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Int)
serviceInfo_count = Lens.lens (\ServiceInfo' {count} -> count) (\s@ServiceInfo' {} a -> s {count = a} :: ServiceInfo)

-- | The first-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
serviceInfo_eventFirstSeen :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_eventFirstSeen = Lens.lens (\ServiceInfo' {eventFirstSeen} -> eventFirstSeen) (\s@ServiceInfo' {} a -> s {eventFirstSeen = a} :: ServiceInfo)

-- | Information about the activity that is described in a finding.
serviceInfo_action :: Lens.Lens' ServiceInfo (Prelude.Maybe Action)
serviceInfo_action = Lens.lens (\ServiceInfo' {action} -> action) (\s@ServiceInfo' {} a -> s {action = a} :: ServiceInfo)

-- | The detector ID for the GuardDuty service.
serviceInfo_detectorId :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_detectorId = Lens.lens (\ServiceInfo' {detectorId} -> detectorId) (\s@ServiceInfo' {} a -> s {detectorId = a} :: ServiceInfo)

-- | The name of the AWS service (GuardDuty) that generated a finding.
serviceInfo_serviceName :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_serviceName = Lens.lens (\ServiceInfo' {serviceName} -> serviceName) (\s@ServiceInfo' {} a -> s {serviceName = a} :: ServiceInfo)

-- | Feedback that was submitted about the finding.
serviceInfo_userFeedback :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_userFeedback = Lens.lens (\ServiceInfo' {userFeedback} -> userFeedback) (\s@ServiceInfo' {} a -> s {userFeedback = a} :: ServiceInfo)

-- | An evidence object associated with the service.
serviceInfo_evidence :: Lens.Lens' ServiceInfo (Prelude.Maybe Evidence)
serviceInfo_evidence = Lens.lens (\ServiceInfo' {evidence} -> evidence) (\s@ServiceInfo' {} a -> s {evidence = a} :: ServiceInfo)

-- | The last-seen timestamp of the activity that prompted GuardDuty to
-- generate this finding.
serviceInfo_eventLastSeen :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_eventLastSeen = Lens.lens (\ServiceInfo' {eventLastSeen} -> eventLastSeen) (\s@ServiceInfo' {} a -> s {eventLastSeen = a} :: ServiceInfo)

-- | The resource role information for this finding.
serviceInfo_resourceRole :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_resourceRole = Lens.lens (\ServiceInfo' {resourceRole} -> resourceRole) (\s@ServiceInfo' {} a -> s {resourceRole = a} :: ServiceInfo)

-- | Indicates whether this finding is archived.
serviceInfo_archived :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Bool)
serviceInfo_archived = Lens.lens (\ServiceInfo' {archived} -> archived) (\s@ServiceInfo' {} a -> s {archived = a} :: ServiceInfo)

instance Core.FromJSON ServiceInfo where
  parseJSON =
    Core.withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            Prelude.<$> (x Core..:? "count")
            Prelude.<*> (x Core..:? "eventFirstSeen")
            Prelude.<*> (x Core..:? "action")
            Prelude.<*> (x Core..:? "detectorId")
            Prelude.<*> (x Core..:? "serviceName")
            Prelude.<*> (x Core..:? "userFeedback")
            Prelude.<*> (x Core..:? "evidence")
            Prelude.<*> (x Core..:? "eventLastSeen")
            Prelude.<*> (x Core..:? "resourceRole")
            Prelude.<*> (x Core..:? "archived")
      )

instance Prelude.Hashable ServiceInfo where
  hashWithSalt salt' ServiceInfo' {..} =
    salt' `Prelude.hashWithSalt` archived
      `Prelude.hashWithSalt` resourceRole
      `Prelude.hashWithSalt` eventLastSeen
      `Prelude.hashWithSalt` evidence
      `Prelude.hashWithSalt` userFeedback
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` eventFirstSeen
      `Prelude.hashWithSalt` count

instance Prelude.NFData ServiceInfo where
  rnf ServiceInfo' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf archived
      `Prelude.seq` Prelude.rnf resourceRole
      `Prelude.seq` Prelude.rnf eventLastSeen
      `Prelude.seq` Prelude.rnf evidence
      `Prelude.seq` Prelude.rnf userFeedback
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf eventFirstSeen
