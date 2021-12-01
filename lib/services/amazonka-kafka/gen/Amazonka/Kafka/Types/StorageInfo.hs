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
-- Module      : Amazonka.Kafka.Types.StorageInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.StorageInfo where

import qualified Amazonka.Core as Core
import Amazonka.Kafka.Types.EBSStorageInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about storage volumes attached to MSK broker nodes.
--
-- /See:/ 'newStorageInfo' smart constructor.
data StorageInfo = StorageInfo'
  { -- | EBS volume information.
    ebsStorageInfo :: Prelude.Maybe EBSStorageInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsStorageInfo', 'storageInfo_ebsStorageInfo' - EBS volume information.
newStorageInfo ::
  StorageInfo
newStorageInfo =
  StorageInfo' {ebsStorageInfo = Prelude.Nothing}

-- | EBS volume information.
storageInfo_ebsStorageInfo :: Lens.Lens' StorageInfo (Prelude.Maybe EBSStorageInfo)
storageInfo_ebsStorageInfo = Lens.lens (\StorageInfo' {ebsStorageInfo} -> ebsStorageInfo) (\s@StorageInfo' {} a -> s {ebsStorageInfo = a} :: StorageInfo)

instance Core.FromJSON StorageInfo where
  parseJSON =
    Core.withObject
      "StorageInfo"
      ( \x ->
          StorageInfo'
            Prelude.<$> (x Core..:? "ebsStorageInfo")
      )

instance Prelude.Hashable StorageInfo where
  hashWithSalt salt' StorageInfo' {..} =
    salt' `Prelude.hashWithSalt` ebsStorageInfo

instance Prelude.NFData StorageInfo where
  rnf StorageInfo' {..} = Prelude.rnf ebsStorageInfo

instance Core.ToJSON StorageInfo where
  toJSON StorageInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ebsStorageInfo" Core..=)
              Prelude.<$> ebsStorageInfo
          ]
      )
