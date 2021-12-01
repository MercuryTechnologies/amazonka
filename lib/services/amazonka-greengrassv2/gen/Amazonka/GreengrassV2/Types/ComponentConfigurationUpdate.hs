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
-- Module      : Amazonka.GreengrassV2.Types.ComponentConfigurationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.ComponentConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a deployment\'s update to a component\'s
-- configuration on Greengrass core devices. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html Update component configurations>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- /See:/ 'newComponentConfigurationUpdate' smart constructor.
data ComponentConfigurationUpdate = ComponentConfigurationUpdate'
  { -- | The list of configuration nodes to reset to default values on target
    -- devices. Use JSON pointers to specify each node to reset. JSON pointers
    -- start with a forward slash (@\/@) and use forward slashes to separate
    -- the key for each level in the object. For more information, see the
    -- <https://tools.ietf.org/html/rfc6901 JSON pointer specification> and
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html#reset-configuration-update Reset configuration updates>
    -- in the /IoT Greengrass V2 Developer Guide/.
    reset :: Prelude.Maybe [Prelude.Text],
    -- | A serialized JSON string that contains the configuration object to merge
    -- to target devices. The core device merges this configuration with the
    -- component\'s existing configuration. If this is the first time a
    -- component deploys on a device, the core device merges this configuration
    -- with the component\'s default configuration. This means that the core
    -- device keeps it\'s existing configuration for keys and values that you
    -- don\'t specify in this object. For more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html#merge-configuration-update Merge configuration updates>
    -- in the /IoT Greengrass V2 Developer Guide/.
    merge :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reset', 'componentConfigurationUpdate_reset' - The list of configuration nodes to reset to default values on target
-- devices. Use JSON pointers to specify each node to reset. JSON pointers
-- start with a forward slash (@\/@) and use forward slashes to separate
-- the key for each level in the object. For more information, see the
-- <https://tools.ietf.org/html/rfc6901 JSON pointer specification> and
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html#reset-configuration-update Reset configuration updates>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'merge', 'componentConfigurationUpdate_merge' - A serialized JSON string that contains the configuration object to merge
-- to target devices. The core device merges this configuration with the
-- component\'s existing configuration. If this is the first time a
-- component deploys on a device, the core device merges this configuration
-- with the component\'s default configuration. This means that the core
-- device keeps it\'s existing configuration for keys and values that you
-- don\'t specify in this object. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html#merge-configuration-update Merge configuration updates>
-- in the /IoT Greengrass V2 Developer Guide/.
newComponentConfigurationUpdate ::
  ComponentConfigurationUpdate
newComponentConfigurationUpdate =
  ComponentConfigurationUpdate'
    { reset =
        Prelude.Nothing,
      merge = Prelude.Nothing
    }

-- | The list of configuration nodes to reset to default values on target
-- devices. Use JSON pointers to specify each node to reset. JSON pointers
-- start with a forward slash (@\/@) and use forward slashes to separate
-- the key for each level in the object. For more information, see the
-- <https://tools.ietf.org/html/rfc6901 JSON pointer specification> and
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html#reset-configuration-update Reset configuration updates>
-- in the /IoT Greengrass V2 Developer Guide/.
componentConfigurationUpdate_reset :: Lens.Lens' ComponentConfigurationUpdate (Prelude.Maybe [Prelude.Text])
componentConfigurationUpdate_reset = Lens.lens (\ComponentConfigurationUpdate' {reset} -> reset) (\s@ComponentConfigurationUpdate' {} a -> s {reset = a} :: ComponentConfigurationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | A serialized JSON string that contains the configuration object to merge
-- to target devices. The core device merges this configuration with the
-- component\'s existing configuration. If this is the first time a
-- component deploys on a device, the core device merges this configuration
-- with the component\'s default configuration. This means that the core
-- device keeps it\'s existing configuration for keys and values that you
-- don\'t specify in this object. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/update-component-configurations.html#merge-configuration-update Merge configuration updates>
-- in the /IoT Greengrass V2 Developer Guide/.
componentConfigurationUpdate_merge :: Lens.Lens' ComponentConfigurationUpdate (Prelude.Maybe Prelude.Text)
componentConfigurationUpdate_merge = Lens.lens (\ComponentConfigurationUpdate' {merge} -> merge) (\s@ComponentConfigurationUpdate' {} a -> s {merge = a} :: ComponentConfigurationUpdate)

instance Core.FromJSON ComponentConfigurationUpdate where
  parseJSON =
    Core.withObject
      "ComponentConfigurationUpdate"
      ( \x ->
          ComponentConfigurationUpdate'
            Prelude.<$> (x Core..:? "reset" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "merge")
      )

instance
  Prelude.Hashable
    ComponentConfigurationUpdate
  where
  hashWithSalt salt' ComponentConfigurationUpdate' {..} =
    salt' `Prelude.hashWithSalt` merge
      `Prelude.hashWithSalt` reset

instance Prelude.NFData ComponentConfigurationUpdate where
  rnf ComponentConfigurationUpdate' {..} =
    Prelude.rnf reset `Prelude.seq` Prelude.rnf merge

instance Core.ToJSON ComponentConfigurationUpdate where
  toJSON ComponentConfigurationUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("reset" Core..=) Prelude.<$> reset,
            ("merge" Core..=) Prelude.<$> merge
          ]
      )
