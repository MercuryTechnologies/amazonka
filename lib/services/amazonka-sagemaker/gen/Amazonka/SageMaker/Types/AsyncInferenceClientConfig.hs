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
-- Module      : Amazonka.SageMaker.Types.AsyncInferenceClientConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AsyncInferenceClientConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configures the behavior of the client used by Amazon SageMaker to
-- interact with the model container during asynchronous inference.
--
-- /See:/ 'newAsyncInferenceClientConfig' smart constructor.
data AsyncInferenceClientConfig = AsyncInferenceClientConfig'
  { -- | The maximum number of concurrent requests sent by the SageMaker client
    -- to the model container. If no value is provided, Amazon SageMaker will
    -- choose an optimal value for you.
    maxConcurrentInvocationsPerInstance :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AsyncInferenceClientConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxConcurrentInvocationsPerInstance', 'asyncInferenceClientConfig_maxConcurrentInvocationsPerInstance' - The maximum number of concurrent requests sent by the SageMaker client
-- to the model container. If no value is provided, Amazon SageMaker will
-- choose an optimal value for you.
newAsyncInferenceClientConfig ::
  AsyncInferenceClientConfig
newAsyncInferenceClientConfig =
  AsyncInferenceClientConfig'
    { maxConcurrentInvocationsPerInstance =
        Prelude.Nothing
    }

-- | The maximum number of concurrent requests sent by the SageMaker client
-- to the model container. If no value is provided, Amazon SageMaker will
-- choose an optimal value for you.
asyncInferenceClientConfig_maxConcurrentInvocationsPerInstance :: Lens.Lens' AsyncInferenceClientConfig (Prelude.Maybe Prelude.Natural)
asyncInferenceClientConfig_maxConcurrentInvocationsPerInstance = Lens.lens (\AsyncInferenceClientConfig' {maxConcurrentInvocationsPerInstance} -> maxConcurrentInvocationsPerInstance) (\s@AsyncInferenceClientConfig' {} a -> s {maxConcurrentInvocationsPerInstance = a} :: AsyncInferenceClientConfig)

instance Core.FromJSON AsyncInferenceClientConfig where
  parseJSON =
    Core.withObject
      "AsyncInferenceClientConfig"
      ( \x ->
          AsyncInferenceClientConfig'
            Prelude.<$> (x Core..:? "MaxConcurrentInvocationsPerInstance")
      )

instance Prelude.Hashable AsyncInferenceClientConfig where
  hashWithSalt salt' AsyncInferenceClientConfig' {..} =
    salt'
      `Prelude.hashWithSalt` maxConcurrentInvocationsPerInstance

instance Prelude.NFData AsyncInferenceClientConfig where
  rnf AsyncInferenceClientConfig' {..} =
    Prelude.rnf maxConcurrentInvocationsPerInstance

instance Core.ToJSON AsyncInferenceClientConfig where
  toJSON AsyncInferenceClientConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxConcurrentInvocationsPerInstance" Core..=)
              Prelude.<$> maxConcurrentInvocationsPerInstance
          ]
      )
