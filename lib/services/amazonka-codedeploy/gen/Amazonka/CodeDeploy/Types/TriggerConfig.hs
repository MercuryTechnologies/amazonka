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
-- Module      : Amazonka.CodeDeploy.Types.TriggerConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TriggerConfig where

import Amazonka.CodeDeploy.Types.TriggerEventType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about notification triggers for the deployment group.
--
-- /See:/ 'newTriggerConfig' smart constructor.
data TriggerConfig = TriggerConfig'
  { -- | The name of the notification trigger.
    triggerName :: Prelude.Maybe Prelude.Text,
    -- | The event type or types for which notifications are triggered.
    triggerEvents :: Prelude.Maybe [TriggerEventType],
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
    -- topic through which notifications about deployment or instance events
    -- are sent.
    triggerTargetArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TriggerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggerName', 'triggerConfig_triggerName' - The name of the notification trigger.
--
-- 'triggerEvents', 'triggerConfig_triggerEvents' - The event type or types for which notifications are triggered.
--
-- 'triggerTargetArn', 'triggerConfig_triggerTargetArn' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- topic through which notifications about deployment or instance events
-- are sent.
newTriggerConfig ::
  TriggerConfig
newTriggerConfig =
  TriggerConfig'
    { triggerName = Prelude.Nothing,
      triggerEvents = Prelude.Nothing,
      triggerTargetArn = Prelude.Nothing
    }

-- | The name of the notification trigger.
triggerConfig_triggerName :: Lens.Lens' TriggerConfig (Prelude.Maybe Prelude.Text)
triggerConfig_triggerName = Lens.lens (\TriggerConfig' {triggerName} -> triggerName) (\s@TriggerConfig' {} a -> s {triggerName = a} :: TriggerConfig)

-- | The event type or types for which notifications are triggered.
triggerConfig_triggerEvents :: Lens.Lens' TriggerConfig (Prelude.Maybe [TriggerEventType])
triggerConfig_triggerEvents = Lens.lens (\TriggerConfig' {triggerEvents} -> triggerEvents) (\s@TriggerConfig' {} a -> s {triggerEvents = a} :: TriggerConfig) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- topic through which notifications about deployment or instance events
-- are sent.
triggerConfig_triggerTargetArn :: Lens.Lens' TriggerConfig (Prelude.Maybe Prelude.Text)
triggerConfig_triggerTargetArn = Lens.lens (\TriggerConfig' {triggerTargetArn} -> triggerTargetArn) (\s@TriggerConfig' {} a -> s {triggerTargetArn = a} :: TriggerConfig)

instance Core.FromJSON TriggerConfig where
  parseJSON =
    Core.withObject
      "TriggerConfig"
      ( \x ->
          TriggerConfig'
            Prelude.<$> (x Core..:? "triggerName")
            Prelude.<*> (x Core..:? "triggerEvents" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "triggerTargetArn")
      )

instance Prelude.Hashable TriggerConfig where
  hashWithSalt salt' TriggerConfig' {..} =
    salt' `Prelude.hashWithSalt` triggerTargetArn
      `Prelude.hashWithSalt` triggerEvents
      `Prelude.hashWithSalt` triggerName

instance Prelude.NFData TriggerConfig where
  rnf TriggerConfig' {..} =
    Prelude.rnf triggerName
      `Prelude.seq` Prelude.rnf triggerTargetArn
      `Prelude.seq` Prelude.rnf triggerEvents

instance Core.ToJSON TriggerConfig where
  toJSON TriggerConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("triggerName" Core..=) Prelude.<$> triggerName,
            ("triggerEvents" Core..=) Prelude.<$> triggerEvents,
            ("triggerTargetArn" Core..=)
              Prelude.<$> triggerTargetArn
          ]
      )
