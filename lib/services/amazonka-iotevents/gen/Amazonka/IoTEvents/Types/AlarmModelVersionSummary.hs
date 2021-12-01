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
-- Module      : Amazonka.IoTEvents.Types.AlarmModelVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AlarmModelVersionSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoTEvents.Types.AlarmModelVersionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of an alarm model version.
--
-- /See:/ 'newAlarmModelVersionSummary' smart constructor.
data AlarmModelVersionSummary = AlarmModelVersionSummary'
  { -- | The time the alarm model was created, in the Unix epoch format.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the alarm model. The status can be one of the following
    -- values:
    --
    -- -   @ACTIVE@ - The alarm model is active and it\'s ready to evaluate
    --     data.
    --
    -- -   @ACTIVATING@ - AWS IoT Events is activating your alarm model.
    --     Activating an alarm model can take up to a few minutes.
    --
    -- -   @INACTIVE@ - The alarm model is inactive, so it isn\'t ready to
    --     evaluate data. Check your alarm model information and update the
    --     alarm model.
    --
    -- -   @FAILED@ - You couldn\'t create or update the alarm model. Check
    --     your alarm model information and try again.
    status :: Prelude.Maybe AlarmModelVersionStatus,
    -- | The name of the alarm model.
    alarmModelName :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the status of the alarm model version.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The time the alarm model was last updated, in the Unix epoch format.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The version of the alarm model.
    alarmModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the alarm model. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    alarmModelArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the IAM role that allows the alarm to perform actions and
    -- access AWS resources. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlarmModelVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'alarmModelVersionSummary_creationTime' - The time the alarm model was created, in the Unix epoch format.
--
-- 'status', 'alarmModelVersionSummary_status' - The status of the alarm model. The status can be one of the following
-- values:
--
-- -   @ACTIVE@ - The alarm model is active and it\'s ready to evaluate
--     data.
--
-- -   @ACTIVATING@ - AWS IoT Events is activating your alarm model.
--     Activating an alarm model can take up to a few minutes.
--
-- -   @INACTIVE@ - The alarm model is inactive, so it isn\'t ready to
--     evaluate data. Check your alarm model information and update the
--     alarm model.
--
-- -   @FAILED@ - You couldn\'t create or update the alarm model. Check
--     your alarm model information and try again.
--
-- 'alarmModelName', 'alarmModelVersionSummary_alarmModelName' - The name of the alarm model.
--
-- 'statusMessage', 'alarmModelVersionSummary_statusMessage' - Contains information about the status of the alarm model version.
--
-- 'lastUpdateTime', 'alarmModelVersionSummary_lastUpdateTime' - The time the alarm model was last updated, in the Unix epoch format.
--
-- 'alarmModelVersion', 'alarmModelVersionSummary_alarmModelVersion' - The version of the alarm model.
--
-- 'alarmModelArn', 'alarmModelVersionSummary_alarmModelArn' - The ARN of the alarm model. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'roleArn', 'alarmModelVersionSummary_roleArn' - The ARN of the IAM role that allows the alarm to perform actions and
-- access AWS resources. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
newAlarmModelVersionSummary ::
  AlarmModelVersionSummary
newAlarmModelVersionSummary =
  AlarmModelVersionSummary'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      alarmModelName = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      alarmModelVersion = Prelude.Nothing,
      alarmModelArn = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The time the alarm model was created, in the Unix epoch format.
alarmModelVersionSummary_creationTime :: Lens.Lens' AlarmModelVersionSummary (Prelude.Maybe Prelude.UTCTime)
alarmModelVersionSummary_creationTime = Lens.lens (\AlarmModelVersionSummary' {creationTime} -> creationTime) (\s@AlarmModelVersionSummary' {} a -> s {creationTime = a} :: AlarmModelVersionSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the alarm model. The status can be one of the following
-- values:
--
-- -   @ACTIVE@ - The alarm model is active and it\'s ready to evaluate
--     data.
--
-- -   @ACTIVATING@ - AWS IoT Events is activating your alarm model.
--     Activating an alarm model can take up to a few minutes.
--
-- -   @INACTIVE@ - The alarm model is inactive, so it isn\'t ready to
--     evaluate data. Check your alarm model information and update the
--     alarm model.
--
-- -   @FAILED@ - You couldn\'t create or update the alarm model. Check
--     your alarm model information and try again.
alarmModelVersionSummary_status :: Lens.Lens' AlarmModelVersionSummary (Prelude.Maybe AlarmModelVersionStatus)
alarmModelVersionSummary_status = Lens.lens (\AlarmModelVersionSummary' {status} -> status) (\s@AlarmModelVersionSummary' {} a -> s {status = a} :: AlarmModelVersionSummary)

-- | The name of the alarm model.
alarmModelVersionSummary_alarmModelName :: Lens.Lens' AlarmModelVersionSummary (Prelude.Maybe Prelude.Text)
alarmModelVersionSummary_alarmModelName = Lens.lens (\AlarmModelVersionSummary' {alarmModelName} -> alarmModelName) (\s@AlarmModelVersionSummary' {} a -> s {alarmModelName = a} :: AlarmModelVersionSummary)

-- | Contains information about the status of the alarm model version.
alarmModelVersionSummary_statusMessage :: Lens.Lens' AlarmModelVersionSummary (Prelude.Maybe Prelude.Text)
alarmModelVersionSummary_statusMessage = Lens.lens (\AlarmModelVersionSummary' {statusMessage} -> statusMessage) (\s@AlarmModelVersionSummary' {} a -> s {statusMessage = a} :: AlarmModelVersionSummary)

-- | The time the alarm model was last updated, in the Unix epoch format.
alarmModelVersionSummary_lastUpdateTime :: Lens.Lens' AlarmModelVersionSummary (Prelude.Maybe Prelude.UTCTime)
alarmModelVersionSummary_lastUpdateTime = Lens.lens (\AlarmModelVersionSummary' {lastUpdateTime} -> lastUpdateTime) (\s@AlarmModelVersionSummary' {} a -> s {lastUpdateTime = a} :: AlarmModelVersionSummary) Prelude.. Lens.mapping Core._Time

-- | The version of the alarm model.
alarmModelVersionSummary_alarmModelVersion :: Lens.Lens' AlarmModelVersionSummary (Prelude.Maybe Prelude.Text)
alarmModelVersionSummary_alarmModelVersion = Lens.lens (\AlarmModelVersionSummary' {alarmModelVersion} -> alarmModelVersion) (\s@AlarmModelVersionSummary' {} a -> s {alarmModelVersion = a} :: AlarmModelVersionSummary)

-- | The ARN of the alarm model. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
alarmModelVersionSummary_alarmModelArn :: Lens.Lens' AlarmModelVersionSummary (Prelude.Maybe Prelude.Text)
alarmModelVersionSummary_alarmModelArn = Lens.lens (\AlarmModelVersionSummary' {alarmModelArn} -> alarmModelArn) (\s@AlarmModelVersionSummary' {} a -> s {alarmModelArn = a} :: AlarmModelVersionSummary)

-- | The ARN of the IAM role that allows the alarm to perform actions and
-- access AWS resources. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
alarmModelVersionSummary_roleArn :: Lens.Lens' AlarmModelVersionSummary (Prelude.Maybe Prelude.Text)
alarmModelVersionSummary_roleArn = Lens.lens (\AlarmModelVersionSummary' {roleArn} -> roleArn) (\s@AlarmModelVersionSummary' {} a -> s {roleArn = a} :: AlarmModelVersionSummary)

instance Core.FromJSON AlarmModelVersionSummary where
  parseJSON =
    Core.withObject
      "AlarmModelVersionSummary"
      ( \x ->
          AlarmModelVersionSummary'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "alarmModelName")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "alarmModelVersion")
            Prelude.<*> (x Core..:? "alarmModelArn")
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable AlarmModelVersionSummary where
  hashWithSalt salt' AlarmModelVersionSummary' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` alarmModelArn
      `Prelude.hashWithSalt` alarmModelVersion
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` alarmModelName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData AlarmModelVersionSummary where
  rnf AlarmModelVersionSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf alarmModelArn
      `Prelude.seq` Prelude.rnf alarmModelVersion
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf alarmModelName
      `Prelude.seq` Prelude.rnf status
