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
-- Module      : Amazonka.IoTEventsData.Types.CustomerAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.CustomerAction where

import qualified Amazonka.Core as Core
import Amazonka.IoTEventsData.Types.AcknowledgeActionConfiguration
import Amazonka.IoTEventsData.Types.CustomerActionName
import Amazonka.IoTEventsData.Types.DisableActionConfiguration
import Amazonka.IoTEventsData.Types.EnableActionConfiguration
import Amazonka.IoTEventsData.Types.ResetActionConfiguration
import Amazonka.IoTEventsData.Types.SnoozeActionConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the action that you can take to respond to
-- the alarm.
--
-- /See:/ 'newCustomerAction' smart constructor.
data CustomerAction = CustomerAction'
  { -- | Contains the configuration information of a reset action.
    resetActionConfiguration :: Prelude.Maybe ResetActionConfiguration,
    -- | The name of the action. The action name can be one of the following
    -- values:
    --
    -- -   @SNOOZE@ - When you snooze the alarm, the alarm state changes to
    --     @SNOOZE_DISABLED@.
    --
    -- -   @ENABLE@ - When you enable the alarm, the alarm state changes to
    --     @NORMAL@.
    --
    -- -   @DISABLE@ - When you disable the alarm, the alarm state changes to
    --     @DISABLED@.
    --
    -- -   @ACKNOWLEDGE@ - When you acknowledge the alarm, the alarm state
    --     changes to @ACKNOWLEDGED@.
    --
    -- -   @RESET@ - When you reset the alarm, the alarm state changes to
    --     @NORMAL@.
    --
    -- For more information, see the
    -- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_AlarmState.html AlarmState>
    -- API.
    actionName :: Prelude.Maybe CustomerActionName,
    -- | Contains the configuration information of a snooze action.
    snoozeActionConfiguration :: Prelude.Maybe SnoozeActionConfiguration,
    -- | Contains the configuration information of an enable action.
    enableActionConfiguration :: Prelude.Maybe EnableActionConfiguration,
    -- | Contains the configuration information of a disable action.
    disableActionConfiguration :: Prelude.Maybe DisableActionConfiguration,
    -- | Contains the configuration information of an acknowledge action.
    acknowledgeActionConfiguration :: Prelude.Maybe AcknowledgeActionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resetActionConfiguration', 'customerAction_resetActionConfiguration' - Contains the configuration information of a reset action.
--
-- 'actionName', 'customerAction_actionName' - The name of the action. The action name can be one of the following
-- values:
--
-- -   @SNOOZE@ - When you snooze the alarm, the alarm state changes to
--     @SNOOZE_DISABLED@.
--
-- -   @ENABLE@ - When you enable the alarm, the alarm state changes to
--     @NORMAL@.
--
-- -   @DISABLE@ - When you disable the alarm, the alarm state changes to
--     @DISABLED@.
--
-- -   @ACKNOWLEDGE@ - When you acknowledge the alarm, the alarm state
--     changes to @ACKNOWLEDGED@.
--
-- -   @RESET@ - When you reset the alarm, the alarm state changes to
--     @NORMAL@.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_AlarmState.html AlarmState>
-- API.
--
-- 'snoozeActionConfiguration', 'customerAction_snoozeActionConfiguration' - Contains the configuration information of a snooze action.
--
-- 'enableActionConfiguration', 'customerAction_enableActionConfiguration' - Contains the configuration information of an enable action.
--
-- 'disableActionConfiguration', 'customerAction_disableActionConfiguration' - Contains the configuration information of a disable action.
--
-- 'acknowledgeActionConfiguration', 'customerAction_acknowledgeActionConfiguration' - Contains the configuration information of an acknowledge action.
newCustomerAction ::
  CustomerAction
newCustomerAction =
  CustomerAction'
    { resetActionConfiguration =
        Prelude.Nothing,
      actionName = Prelude.Nothing,
      snoozeActionConfiguration = Prelude.Nothing,
      enableActionConfiguration = Prelude.Nothing,
      disableActionConfiguration = Prelude.Nothing,
      acknowledgeActionConfiguration = Prelude.Nothing
    }

-- | Contains the configuration information of a reset action.
customerAction_resetActionConfiguration :: Lens.Lens' CustomerAction (Prelude.Maybe ResetActionConfiguration)
customerAction_resetActionConfiguration = Lens.lens (\CustomerAction' {resetActionConfiguration} -> resetActionConfiguration) (\s@CustomerAction' {} a -> s {resetActionConfiguration = a} :: CustomerAction)

-- | The name of the action. The action name can be one of the following
-- values:
--
-- -   @SNOOZE@ - When you snooze the alarm, the alarm state changes to
--     @SNOOZE_DISABLED@.
--
-- -   @ENABLE@ - When you enable the alarm, the alarm state changes to
--     @NORMAL@.
--
-- -   @DISABLE@ - When you disable the alarm, the alarm state changes to
--     @DISABLED@.
--
-- -   @ACKNOWLEDGE@ - When you acknowledge the alarm, the alarm state
--     changes to @ACKNOWLEDGED@.
--
-- -   @RESET@ - When you reset the alarm, the alarm state changes to
--     @NORMAL@.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/iotevents/latest/apireference/API_iotevents-data_AlarmState.html AlarmState>
-- API.
customerAction_actionName :: Lens.Lens' CustomerAction (Prelude.Maybe CustomerActionName)
customerAction_actionName = Lens.lens (\CustomerAction' {actionName} -> actionName) (\s@CustomerAction' {} a -> s {actionName = a} :: CustomerAction)

-- | Contains the configuration information of a snooze action.
customerAction_snoozeActionConfiguration :: Lens.Lens' CustomerAction (Prelude.Maybe SnoozeActionConfiguration)
customerAction_snoozeActionConfiguration = Lens.lens (\CustomerAction' {snoozeActionConfiguration} -> snoozeActionConfiguration) (\s@CustomerAction' {} a -> s {snoozeActionConfiguration = a} :: CustomerAction)

-- | Contains the configuration information of an enable action.
customerAction_enableActionConfiguration :: Lens.Lens' CustomerAction (Prelude.Maybe EnableActionConfiguration)
customerAction_enableActionConfiguration = Lens.lens (\CustomerAction' {enableActionConfiguration} -> enableActionConfiguration) (\s@CustomerAction' {} a -> s {enableActionConfiguration = a} :: CustomerAction)

-- | Contains the configuration information of a disable action.
customerAction_disableActionConfiguration :: Lens.Lens' CustomerAction (Prelude.Maybe DisableActionConfiguration)
customerAction_disableActionConfiguration = Lens.lens (\CustomerAction' {disableActionConfiguration} -> disableActionConfiguration) (\s@CustomerAction' {} a -> s {disableActionConfiguration = a} :: CustomerAction)

-- | Contains the configuration information of an acknowledge action.
customerAction_acknowledgeActionConfiguration :: Lens.Lens' CustomerAction (Prelude.Maybe AcknowledgeActionConfiguration)
customerAction_acknowledgeActionConfiguration = Lens.lens (\CustomerAction' {acknowledgeActionConfiguration} -> acknowledgeActionConfiguration) (\s@CustomerAction' {} a -> s {acknowledgeActionConfiguration = a} :: CustomerAction)

instance Core.FromJSON CustomerAction where
  parseJSON =
    Core.withObject
      "CustomerAction"
      ( \x ->
          CustomerAction'
            Prelude.<$> (x Core..:? "resetActionConfiguration")
            Prelude.<*> (x Core..:? "actionName")
            Prelude.<*> (x Core..:? "snoozeActionConfiguration")
            Prelude.<*> (x Core..:? "enableActionConfiguration")
            Prelude.<*> (x Core..:? "disableActionConfiguration")
            Prelude.<*> (x Core..:? "acknowledgeActionConfiguration")
      )

instance Prelude.Hashable CustomerAction where
  hashWithSalt salt' CustomerAction' {..} =
    salt'
      `Prelude.hashWithSalt` acknowledgeActionConfiguration
      `Prelude.hashWithSalt` disableActionConfiguration
      `Prelude.hashWithSalt` enableActionConfiguration
      `Prelude.hashWithSalt` snoozeActionConfiguration
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` resetActionConfiguration

instance Prelude.NFData CustomerAction where
  rnf CustomerAction' {..} =
    Prelude.rnf resetActionConfiguration
      `Prelude.seq` Prelude.rnf acknowledgeActionConfiguration
      `Prelude.seq` Prelude.rnf disableActionConfiguration
      `Prelude.seq` Prelude.rnf enableActionConfiguration
      `Prelude.seq` Prelude.rnf snoozeActionConfiguration
      `Prelude.seq` Prelude.rnf actionName
