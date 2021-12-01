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
-- Module      : Amazonka.XRay.Types.ErrorRootCauseEntity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ErrorRootCauseEntity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.RootCauseException

-- | A collection of segments and corresponding subsegments associated to a
-- trace summary error.
--
-- /See:/ 'newErrorRootCauseEntity' smart constructor.
data ErrorRootCauseEntity = ErrorRootCauseEntity'
  { -- | The types and messages of the exceptions.
    exceptions :: Prelude.Maybe [RootCauseException],
    -- | A flag that denotes a remote subsegment.
    remote :: Prelude.Maybe Prelude.Bool,
    -- | The name of the entity.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorRootCauseEntity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exceptions', 'errorRootCauseEntity_exceptions' - The types and messages of the exceptions.
--
-- 'remote', 'errorRootCauseEntity_remote' - A flag that denotes a remote subsegment.
--
-- 'name', 'errorRootCauseEntity_name' - The name of the entity.
newErrorRootCauseEntity ::
  ErrorRootCauseEntity
newErrorRootCauseEntity =
  ErrorRootCauseEntity'
    { exceptions = Prelude.Nothing,
      remote = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The types and messages of the exceptions.
errorRootCauseEntity_exceptions :: Lens.Lens' ErrorRootCauseEntity (Prelude.Maybe [RootCauseException])
errorRootCauseEntity_exceptions = Lens.lens (\ErrorRootCauseEntity' {exceptions} -> exceptions) (\s@ErrorRootCauseEntity' {} a -> s {exceptions = a} :: ErrorRootCauseEntity) Prelude.. Lens.mapping Lens.coerced

-- | A flag that denotes a remote subsegment.
errorRootCauseEntity_remote :: Lens.Lens' ErrorRootCauseEntity (Prelude.Maybe Prelude.Bool)
errorRootCauseEntity_remote = Lens.lens (\ErrorRootCauseEntity' {remote} -> remote) (\s@ErrorRootCauseEntity' {} a -> s {remote = a} :: ErrorRootCauseEntity)

-- | The name of the entity.
errorRootCauseEntity_name :: Lens.Lens' ErrorRootCauseEntity (Prelude.Maybe Prelude.Text)
errorRootCauseEntity_name = Lens.lens (\ErrorRootCauseEntity' {name} -> name) (\s@ErrorRootCauseEntity' {} a -> s {name = a} :: ErrorRootCauseEntity)

instance Core.FromJSON ErrorRootCauseEntity where
  parseJSON =
    Core.withObject
      "ErrorRootCauseEntity"
      ( \x ->
          ErrorRootCauseEntity'
            Prelude.<$> (x Core..:? "Exceptions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Remote")
            Prelude.<*> (x Core..:? "Name")
      )

instance Prelude.Hashable ErrorRootCauseEntity where
  hashWithSalt salt' ErrorRootCauseEntity' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` remote
      `Prelude.hashWithSalt` exceptions

instance Prelude.NFData ErrorRootCauseEntity where
  rnf ErrorRootCauseEntity' {..} =
    Prelude.rnf exceptions
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf remote
