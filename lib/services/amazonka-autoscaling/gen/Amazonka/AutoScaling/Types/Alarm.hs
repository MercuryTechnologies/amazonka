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
-- Module      : Amazonka.AutoScaling.Types.Alarm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.Alarm where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an alarm.
--
-- /See:/ 'newAlarm' smart constructor.
data Alarm = Alarm'
  { -- | The name of the alarm.
    alarmName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the alarm.
    alarmARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Alarm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alarmName', 'alarm_alarmName' - The name of the alarm.
--
-- 'alarmARN', 'alarm_alarmARN' - The Amazon Resource Name (ARN) of the alarm.
newAlarm ::
  Alarm
newAlarm =
  Alarm'
    { alarmName = Prelude.Nothing,
      alarmARN = Prelude.Nothing
    }

-- | The name of the alarm.
alarm_alarmName :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_alarmName = Lens.lens (\Alarm' {alarmName} -> alarmName) (\s@Alarm' {} a -> s {alarmName = a} :: Alarm)

-- | The Amazon Resource Name (ARN) of the alarm.
alarm_alarmARN :: Lens.Lens' Alarm (Prelude.Maybe Prelude.Text)
alarm_alarmARN = Lens.lens (\Alarm' {alarmARN} -> alarmARN) (\s@Alarm' {} a -> s {alarmARN = a} :: Alarm)

instance Core.FromXML Alarm where
  parseXML x =
    Alarm'
      Prelude.<$> (x Core..@? "AlarmName")
      Prelude.<*> (x Core..@? "AlarmARN")

instance Prelude.Hashable Alarm where
  hashWithSalt salt' Alarm' {..} =
    salt' `Prelude.hashWithSalt` alarmARN
      `Prelude.hashWithSalt` alarmName

instance Prelude.NFData Alarm where
  rnf Alarm' {..} =
    Prelude.rnf alarmName
      `Prelude.seq` Prelude.rnf alarmARN
