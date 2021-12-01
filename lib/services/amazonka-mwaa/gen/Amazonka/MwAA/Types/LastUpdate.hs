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
-- Module      : Amazonka.MwAA.Types.LastUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.LastUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MwAA.Types.UpdateError
import Amazonka.MwAA.Types.UpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | The status of the last update on the environment, and any errors that
-- were encountered.
--
-- /See:/ 'newLastUpdate' smart constructor.
data LastUpdate = LastUpdate'
  { -- | The status of the last update on the environment. Valid values:
    -- @SUCCESS@, @PENDING@, @FAILED@.
    status :: Prelude.Maybe UpdateStatus,
    -- | The day and time of the last update on the environment.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The error that was encountered during the last update of the
    -- environment.
    error :: Prelude.Maybe UpdateError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LastUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'lastUpdate_status' - The status of the last update on the environment. Valid values:
-- @SUCCESS@, @PENDING@, @FAILED@.
--
-- 'createdAt', 'lastUpdate_createdAt' - The day and time of the last update on the environment.
--
-- 'error', 'lastUpdate_error' - The error that was encountered during the last update of the
-- environment.
newLastUpdate ::
  LastUpdate
newLastUpdate =
  LastUpdate'
    { status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The status of the last update on the environment. Valid values:
-- @SUCCESS@, @PENDING@, @FAILED@.
lastUpdate_status :: Lens.Lens' LastUpdate (Prelude.Maybe UpdateStatus)
lastUpdate_status = Lens.lens (\LastUpdate' {status} -> status) (\s@LastUpdate' {} a -> s {status = a} :: LastUpdate)

-- | The day and time of the last update on the environment.
lastUpdate_createdAt :: Lens.Lens' LastUpdate (Prelude.Maybe Prelude.UTCTime)
lastUpdate_createdAt = Lens.lens (\LastUpdate' {createdAt} -> createdAt) (\s@LastUpdate' {} a -> s {createdAt = a} :: LastUpdate) Prelude.. Lens.mapping Core._Time

-- | The error that was encountered during the last update of the
-- environment.
lastUpdate_error :: Lens.Lens' LastUpdate (Prelude.Maybe UpdateError)
lastUpdate_error = Lens.lens (\LastUpdate' {error} -> error) (\s@LastUpdate' {} a -> s {error = a} :: LastUpdate)

instance Core.FromJSON LastUpdate where
  parseJSON =
    Core.withObject
      "LastUpdate"
      ( \x ->
          LastUpdate'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Error")
      )

instance Prelude.Hashable LastUpdate where
  hashWithSalt salt' LastUpdate' {..} =
    salt' `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData LastUpdate where
  rnf LastUpdate' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf createdAt
