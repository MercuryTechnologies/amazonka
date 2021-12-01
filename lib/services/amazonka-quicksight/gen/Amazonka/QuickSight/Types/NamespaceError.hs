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
-- Module      : Amazonka.QuickSight.Types.NamespaceError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamespaceError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NamespaceErrorType

-- | Errors that occur during namespace creation.
--
-- /See:/ 'newNamespaceError' smart constructor.
data NamespaceError = NamespaceError'
  { -- | The error type.
    type' :: Prelude.Maybe NamespaceErrorType,
    -- | The message for the error.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NamespaceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'namespaceError_type' - The error type.
--
-- 'message', 'namespaceError_message' - The message for the error.
newNamespaceError ::
  NamespaceError
newNamespaceError =
  NamespaceError'
    { type' = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error type.
namespaceError_type :: Lens.Lens' NamespaceError (Prelude.Maybe NamespaceErrorType)
namespaceError_type = Lens.lens (\NamespaceError' {type'} -> type') (\s@NamespaceError' {} a -> s {type' = a} :: NamespaceError)

-- | The message for the error.
namespaceError_message :: Lens.Lens' NamespaceError (Prelude.Maybe Prelude.Text)
namespaceError_message = Lens.lens (\NamespaceError' {message} -> message) (\s@NamespaceError' {} a -> s {message = a} :: NamespaceError)

instance Core.FromJSON NamespaceError where
  parseJSON =
    Core.withObject
      "NamespaceError"
      ( \x ->
          NamespaceError'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Message")
      )

instance Prelude.Hashable NamespaceError where
  hashWithSalt salt' NamespaceError' {..} =
    salt' `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData NamespaceError where
  rnf NamespaceError' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf message
