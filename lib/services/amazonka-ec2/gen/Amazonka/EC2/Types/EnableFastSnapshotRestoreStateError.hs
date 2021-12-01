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
-- Module      : Amazonka.EC2.Types.EnableFastSnapshotRestoreStateError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.EnableFastSnapshotRestoreStateError where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an error that occurred when enabling fast snapshot restores.
--
-- /See:/ 'newEnableFastSnapshotRestoreStateError' smart constructor.
data EnableFastSnapshotRestoreStateError = EnableFastSnapshotRestoreStateError'
  { -- | The error code.
    code :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableFastSnapshotRestoreStateError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'enableFastSnapshotRestoreStateError_code' - The error code.
--
-- 'message', 'enableFastSnapshotRestoreStateError_message' - The error message.
newEnableFastSnapshotRestoreStateError ::
  EnableFastSnapshotRestoreStateError
newEnableFastSnapshotRestoreStateError =
  EnableFastSnapshotRestoreStateError'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error code.
enableFastSnapshotRestoreStateError_code :: Lens.Lens' EnableFastSnapshotRestoreStateError (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreStateError_code = Lens.lens (\EnableFastSnapshotRestoreStateError' {code} -> code) (\s@EnableFastSnapshotRestoreStateError' {} a -> s {code = a} :: EnableFastSnapshotRestoreStateError)

-- | The error message.
enableFastSnapshotRestoreStateError_message :: Lens.Lens' EnableFastSnapshotRestoreStateError (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreStateError_message = Lens.lens (\EnableFastSnapshotRestoreStateError' {message} -> message) (\s@EnableFastSnapshotRestoreStateError' {} a -> s {message = a} :: EnableFastSnapshotRestoreStateError)

instance
  Core.FromXML
    EnableFastSnapshotRestoreStateError
  where
  parseXML x =
    EnableFastSnapshotRestoreStateError'
      Prelude.<$> (x Core..@? "code")
      Prelude.<*> (x Core..@? "message")

instance
  Prelude.Hashable
    EnableFastSnapshotRestoreStateError
  where
  hashWithSalt
    salt'
    EnableFastSnapshotRestoreStateError' {..} =
      salt' `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` code

instance
  Prelude.NFData
    EnableFastSnapshotRestoreStateError
  where
  rnf EnableFastSnapshotRestoreStateError' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
