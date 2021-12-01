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
-- Module      : Amazonka.Greengrass.Types.LocalDeviceResourceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.LocalDeviceResourceData where

import qualified Amazonka.Core as Core
import Amazonka.Greengrass.Types.GroupOwnerSetting
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Attributes that define a local device resource.
--
-- /See:/ 'newLocalDeviceResourceData' smart constructor.
data LocalDeviceResourceData = LocalDeviceResourceData'
  { -- | Group\/owner related settings for local resources.
    groupOwnerSetting :: Prelude.Maybe GroupOwnerSetting,
    -- | The local absolute path of the device resource. The source path for a
    -- device resource can refer only to a character device or block device
    -- under \'\'\/dev\'\'.
    sourcePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalDeviceResourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupOwnerSetting', 'localDeviceResourceData_groupOwnerSetting' - Group\/owner related settings for local resources.
--
-- 'sourcePath', 'localDeviceResourceData_sourcePath' - The local absolute path of the device resource. The source path for a
-- device resource can refer only to a character device or block device
-- under \'\'\/dev\'\'.
newLocalDeviceResourceData ::
  LocalDeviceResourceData
newLocalDeviceResourceData =
  LocalDeviceResourceData'
    { groupOwnerSetting =
        Prelude.Nothing,
      sourcePath = Prelude.Nothing
    }

-- | Group\/owner related settings for local resources.
localDeviceResourceData_groupOwnerSetting :: Lens.Lens' LocalDeviceResourceData (Prelude.Maybe GroupOwnerSetting)
localDeviceResourceData_groupOwnerSetting = Lens.lens (\LocalDeviceResourceData' {groupOwnerSetting} -> groupOwnerSetting) (\s@LocalDeviceResourceData' {} a -> s {groupOwnerSetting = a} :: LocalDeviceResourceData)

-- | The local absolute path of the device resource. The source path for a
-- device resource can refer only to a character device or block device
-- under \'\'\/dev\'\'.
localDeviceResourceData_sourcePath :: Lens.Lens' LocalDeviceResourceData (Prelude.Maybe Prelude.Text)
localDeviceResourceData_sourcePath = Lens.lens (\LocalDeviceResourceData' {sourcePath} -> sourcePath) (\s@LocalDeviceResourceData' {} a -> s {sourcePath = a} :: LocalDeviceResourceData)

instance Core.FromJSON LocalDeviceResourceData where
  parseJSON =
    Core.withObject
      "LocalDeviceResourceData"
      ( \x ->
          LocalDeviceResourceData'
            Prelude.<$> (x Core..:? "GroupOwnerSetting")
            Prelude.<*> (x Core..:? "SourcePath")
      )

instance Prelude.Hashable LocalDeviceResourceData where
  hashWithSalt salt' LocalDeviceResourceData' {..} =
    salt' `Prelude.hashWithSalt` sourcePath
      `Prelude.hashWithSalt` groupOwnerSetting

instance Prelude.NFData LocalDeviceResourceData where
  rnf LocalDeviceResourceData' {..} =
    Prelude.rnf groupOwnerSetting
      `Prelude.seq` Prelude.rnf sourcePath

instance Core.ToJSON LocalDeviceResourceData where
  toJSON LocalDeviceResourceData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GroupOwnerSetting" Core..=)
              Prelude.<$> groupOwnerSetting,
            ("SourcePath" Core..=) Prelude.<$> sourcePath
          ]
      )
