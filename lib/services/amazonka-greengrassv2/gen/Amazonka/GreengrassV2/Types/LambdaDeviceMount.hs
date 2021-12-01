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
-- Module      : Amazonka.GreengrassV2.Types.LambdaDeviceMount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.LambdaDeviceMount where

import qualified Amazonka.Core as Core
import Amazonka.GreengrassV2.Types.LambdaFilesystemPermission
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a device that Linux processes in a container
-- can access.
--
-- /See:/ 'newLambdaDeviceMount' smart constructor.
data LambdaDeviceMount = LambdaDeviceMount'
  { -- | Whether or not to add the component\'s system user as an owner of the
    -- device.
    --
    -- Default: @false@
    addGroupOwner :: Prelude.Maybe Prelude.Bool,
    -- | The permission to access the device: read\/only (@ro@) or read\/write
    -- (@rw@).
    --
    -- Default: @ro@
    permission :: Prelude.Maybe LambdaFilesystemPermission,
    -- | The mount path for the device in the file system.
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaDeviceMount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addGroupOwner', 'lambdaDeviceMount_addGroupOwner' - Whether or not to add the component\'s system user as an owner of the
-- device.
--
-- Default: @false@
--
-- 'permission', 'lambdaDeviceMount_permission' - The permission to access the device: read\/only (@ro@) or read\/write
-- (@rw@).
--
-- Default: @ro@
--
-- 'path', 'lambdaDeviceMount_path' - The mount path for the device in the file system.
newLambdaDeviceMount ::
  -- | 'path'
  Prelude.Text ->
  LambdaDeviceMount
newLambdaDeviceMount pPath_ =
  LambdaDeviceMount'
    { addGroupOwner = Prelude.Nothing,
      permission = Prelude.Nothing,
      path = pPath_
    }

-- | Whether or not to add the component\'s system user as an owner of the
-- device.
--
-- Default: @false@
lambdaDeviceMount_addGroupOwner :: Lens.Lens' LambdaDeviceMount (Prelude.Maybe Prelude.Bool)
lambdaDeviceMount_addGroupOwner = Lens.lens (\LambdaDeviceMount' {addGroupOwner} -> addGroupOwner) (\s@LambdaDeviceMount' {} a -> s {addGroupOwner = a} :: LambdaDeviceMount)

-- | The permission to access the device: read\/only (@ro@) or read\/write
-- (@rw@).
--
-- Default: @ro@
lambdaDeviceMount_permission :: Lens.Lens' LambdaDeviceMount (Prelude.Maybe LambdaFilesystemPermission)
lambdaDeviceMount_permission = Lens.lens (\LambdaDeviceMount' {permission} -> permission) (\s@LambdaDeviceMount' {} a -> s {permission = a} :: LambdaDeviceMount)

-- | The mount path for the device in the file system.
lambdaDeviceMount_path :: Lens.Lens' LambdaDeviceMount Prelude.Text
lambdaDeviceMount_path = Lens.lens (\LambdaDeviceMount' {path} -> path) (\s@LambdaDeviceMount' {} a -> s {path = a} :: LambdaDeviceMount)

instance Prelude.Hashable LambdaDeviceMount where
  hashWithSalt salt' LambdaDeviceMount' {..} =
    salt' `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` permission
      `Prelude.hashWithSalt` addGroupOwner

instance Prelude.NFData LambdaDeviceMount where
  rnf LambdaDeviceMount' {..} =
    Prelude.rnf addGroupOwner
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf permission

instance Core.ToJSON LambdaDeviceMount where
  toJSON LambdaDeviceMount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("addGroupOwner" Core..=) Prelude.<$> addGroupOwner,
            ("permission" Core..=) Prelude.<$> permission,
            Prelude.Just ("path" Core..= path)
          ]
      )
