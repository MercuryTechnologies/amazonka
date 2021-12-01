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
-- Module      : Amazonka.WorkSpaces.Types.UpdateResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.UpdateResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes whether a WorkSpace image needs to be updated with the latest
-- drivers and other components required by Amazon WorkSpaces.
--
-- Only Windows 10 WorkSpace images can be programmatically updated at this
-- time.
--
-- /See:/ 'newUpdateResult' smart constructor.
data UpdateResult = UpdateResult'
  { -- | Indicates whether updated drivers or other components are available for
    -- the specified WorkSpace image.
    updateAvailable :: Prelude.Maybe Prelude.Bool,
    -- | A description of whether updates for the WorkSpace image are pending or
    -- available.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateAvailable', 'updateResult_updateAvailable' - Indicates whether updated drivers or other components are available for
-- the specified WorkSpace image.
--
-- 'description', 'updateResult_description' - A description of whether updates for the WorkSpace image are pending or
-- available.
newUpdateResult ::
  UpdateResult
newUpdateResult =
  UpdateResult'
    { updateAvailable = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Indicates whether updated drivers or other components are available for
-- the specified WorkSpace image.
updateResult_updateAvailable :: Lens.Lens' UpdateResult (Prelude.Maybe Prelude.Bool)
updateResult_updateAvailable = Lens.lens (\UpdateResult' {updateAvailable} -> updateAvailable) (\s@UpdateResult' {} a -> s {updateAvailable = a} :: UpdateResult)

-- | A description of whether updates for the WorkSpace image are pending or
-- available.
updateResult_description :: Lens.Lens' UpdateResult (Prelude.Maybe Prelude.Text)
updateResult_description = Lens.lens (\UpdateResult' {description} -> description) (\s@UpdateResult' {} a -> s {description = a} :: UpdateResult)

instance Core.FromJSON UpdateResult where
  parseJSON =
    Core.withObject
      "UpdateResult"
      ( \x ->
          UpdateResult'
            Prelude.<$> (x Core..:? "UpdateAvailable")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable UpdateResult where
  hashWithSalt salt' UpdateResult' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` updateAvailable

instance Prelude.NFData UpdateResult where
  rnf UpdateResult' {..} =
    Prelude.rnf updateAvailable
      `Prelude.seq` Prelude.rnf description
