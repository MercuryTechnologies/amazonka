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
-- Module      : Amazonka.AmplifyBackend.Types.BackendAPIConflictResolution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.BackendAPIConflictResolution where

import Amazonka.AmplifyBackend.Types.ResolutionStrategy
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the conflict resolution configuration for your data model
-- configured in your Amplify project.
--
-- /See:/ 'newBackendAPIConflictResolution' smart constructor.
data BackendAPIConflictResolution = BackendAPIConflictResolution'
  { -- | The strategy for conflict resolution.
    resolutionStrategy :: Prelude.Maybe ResolutionStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackendAPIConflictResolution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolutionStrategy', 'backendAPIConflictResolution_resolutionStrategy' - The strategy for conflict resolution.
newBackendAPIConflictResolution ::
  BackendAPIConflictResolution
newBackendAPIConflictResolution =
  BackendAPIConflictResolution'
    { resolutionStrategy =
        Prelude.Nothing
    }

-- | The strategy for conflict resolution.
backendAPIConflictResolution_resolutionStrategy :: Lens.Lens' BackendAPIConflictResolution (Prelude.Maybe ResolutionStrategy)
backendAPIConflictResolution_resolutionStrategy = Lens.lens (\BackendAPIConflictResolution' {resolutionStrategy} -> resolutionStrategy) (\s@BackendAPIConflictResolution' {} a -> s {resolutionStrategy = a} :: BackendAPIConflictResolution)

instance Core.FromJSON BackendAPIConflictResolution where
  parseJSON =
    Core.withObject
      "BackendAPIConflictResolution"
      ( \x ->
          BackendAPIConflictResolution'
            Prelude.<$> (x Core..:? "resolutionStrategy")
      )

instance
  Prelude.Hashable
    BackendAPIConflictResolution
  where
  hashWithSalt salt' BackendAPIConflictResolution' {..} =
    salt' `Prelude.hashWithSalt` resolutionStrategy

instance Prelude.NFData BackendAPIConflictResolution where
  rnf BackendAPIConflictResolution' {..} =
    Prelude.rnf resolutionStrategy

instance Core.ToJSON BackendAPIConflictResolution where
  toJSON BackendAPIConflictResolution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resolutionStrategy" Core..=)
              Prelude.<$> resolutionStrategy
          ]
      )
