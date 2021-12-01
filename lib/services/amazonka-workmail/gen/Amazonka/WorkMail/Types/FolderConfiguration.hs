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
-- Module      : Amazonka.WorkMail.Types.FolderConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.FolderConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkMail.Types.FolderName
import Amazonka.WorkMail.Types.RetentionAction

-- | The configuration applied to an organization\'s folders by its retention
-- policy.
--
-- /See:/ 'newFolderConfiguration' smart constructor.
data FolderConfiguration = FolderConfiguration'
  { -- | The number of days for which the folder-configuration action applies.
    period :: Prelude.Maybe Prelude.Natural,
    -- | The folder name.
    name :: FolderName,
    -- | The action to take on the folder contents at the end of the folder
    -- configuration period.
    action :: RetentionAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FolderConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'period', 'folderConfiguration_period' - The number of days for which the folder-configuration action applies.
--
-- 'name', 'folderConfiguration_name' - The folder name.
--
-- 'action', 'folderConfiguration_action' - The action to take on the folder contents at the end of the folder
-- configuration period.
newFolderConfiguration ::
  -- | 'name'
  FolderName ->
  -- | 'action'
  RetentionAction ->
  FolderConfiguration
newFolderConfiguration pName_ pAction_ =
  FolderConfiguration'
    { period = Prelude.Nothing,
      name = pName_,
      action = pAction_
    }

-- | The number of days for which the folder-configuration action applies.
folderConfiguration_period :: Lens.Lens' FolderConfiguration (Prelude.Maybe Prelude.Natural)
folderConfiguration_period = Lens.lens (\FolderConfiguration' {period} -> period) (\s@FolderConfiguration' {} a -> s {period = a} :: FolderConfiguration)

-- | The folder name.
folderConfiguration_name :: Lens.Lens' FolderConfiguration FolderName
folderConfiguration_name = Lens.lens (\FolderConfiguration' {name} -> name) (\s@FolderConfiguration' {} a -> s {name = a} :: FolderConfiguration)

-- | The action to take on the folder contents at the end of the folder
-- configuration period.
folderConfiguration_action :: Lens.Lens' FolderConfiguration RetentionAction
folderConfiguration_action = Lens.lens (\FolderConfiguration' {action} -> action) (\s@FolderConfiguration' {} a -> s {action = a} :: FolderConfiguration)

instance Core.FromJSON FolderConfiguration where
  parseJSON =
    Core.withObject
      "FolderConfiguration"
      ( \x ->
          FolderConfiguration'
            Prelude.<$> (x Core..:? "Period")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Action")
      )

instance Prelude.Hashable FolderConfiguration where
  hashWithSalt salt' FolderConfiguration' {..} =
    salt' `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` period

instance Prelude.NFData FolderConfiguration where
  rnf FolderConfiguration' {..} =
    Prelude.rnf period `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf name

instance Core.ToJSON FolderConfiguration where
  toJSON FolderConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Period" Core..=) Prelude.<$> period,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Action" Core..= action)
          ]
      )
