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
-- Module      : Amazonka.OpsWorks.Types.StackConfigurationManager
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.StackConfigurationManager where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration manager.
--
-- /See:/ 'newStackConfigurationManager' smart constructor.
data StackConfigurationManager = StackConfigurationManager'
  { -- | The name. This parameter must be set to \"Chef\".
    name :: Prelude.Maybe Prelude.Text,
    -- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for
    -- Linux stacks, and to 12.2 for Windows stacks. The default value for
    -- Linux stacks is 11.4.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackConfigurationManager' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stackConfigurationManager_name' - The name. This parameter must be set to \"Chef\".
--
-- 'version', 'stackConfigurationManager_version' - The Chef version. This parameter must be set to 12, 11.10, or 11.4 for
-- Linux stacks, and to 12.2 for Windows stacks. The default value for
-- Linux stacks is 11.4.
newStackConfigurationManager ::
  StackConfigurationManager
newStackConfigurationManager =
  StackConfigurationManager'
    { name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name. This parameter must be set to \"Chef\".
stackConfigurationManager_name :: Lens.Lens' StackConfigurationManager (Prelude.Maybe Prelude.Text)
stackConfigurationManager_name = Lens.lens (\StackConfigurationManager' {name} -> name) (\s@StackConfigurationManager' {} a -> s {name = a} :: StackConfigurationManager)

-- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for
-- Linux stacks, and to 12.2 for Windows stacks. The default value for
-- Linux stacks is 11.4.
stackConfigurationManager_version :: Lens.Lens' StackConfigurationManager (Prelude.Maybe Prelude.Text)
stackConfigurationManager_version = Lens.lens (\StackConfigurationManager' {version} -> version) (\s@StackConfigurationManager' {} a -> s {version = a} :: StackConfigurationManager)

instance Core.FromJSON StackConfigurationManager where
  parseJSON =
    Core.withObject
      "StackConfigurationManager"
      ( \x ->
          StackConfigurationManager'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Version")
      )

instance Prelude.Hashable StackConfigurationManager where
  hashWithSalt salt' StackConfigurationManager' {..} =
    salt' `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name

instance Prelude.NFData StackConfigurationManager where
  rnf StackConfigurationManager' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Core.ToJSON StackConfigurationManager where
  toJSON StackConfigurationManager' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Name" Core..=) Prelude.<$> name,
            ("Version" Core..=) Prelude.<$> version
          ]
      )
