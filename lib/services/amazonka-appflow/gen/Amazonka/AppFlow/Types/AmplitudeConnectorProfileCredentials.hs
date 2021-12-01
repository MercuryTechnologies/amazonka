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
-- Module      : Amazonka.AppFlow.Types.AmplitudeConnectorProfileCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.AmplitudeConnectorProfileCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific credentials required when using Amplitude.
--
-- /See:/ 'newAmplitudeConnectorProfileCredentials' smart constructor.
data AmplitudeConnectorProfileCredentials = AmplitudeConnectorProfileCredentials'
  { -- | A unique alphanumeric identifier used to authenticate a user, developer,
    -- or calling program to your API.
    apiKey :: Prelude.Text,
    -- | The Secret Access Key portion of the credentials.
    secretKey :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmplitudeConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKey', 'amplitudeConnectorProfileCredentials_apiKey' - A unique alphanumeric identifier used to authenticate a user, developer,
-- or calling program to your API.
--
-- 'secretKey', 'amplitudeConnectorProfileCredentials_secretKey' - The Secret Access Key portion of the credentials.
newAmplitudeConnectorProfileCredentials ::
  -- | 'apiKey'
  Prelude.Text ->
  -- | 'secretKey'
  Prelude.Text ->
  AmplitudeConnectorProfileCredentials
newAmplitudeConnectorProfileCredentials
  pApiKey_
  pSecretKey_ =
    AmplitudeConnectorProfileCredentials'
      { apiKey =
          pApiKey_,
        secretKey =
          Core._Sensitive Lens.# pSecretKey_
      }

-- | A unique alphanumeric identifier used to authenticate a user, developer,
-- or calling program to your API.
amplitudeConnectorProfileCredentials_apiKey :: Lens.Lens' AmplitudeConnectorProfileCredentials Prelude.Text
amplitudeConnectorProfileCredentials_apiKey = Lens.lens (\AmplitudeConnectorProfileCredentials' {apiKey} -> apiKey) (\s@AmplitudeConnectorProfileCredentials' {} a -> s {apiKey = a} :: AmplitudeConnectorProfileCredentials)

-- | The Secret Access Key portion of the credentials.
amplitudeConnectorProfileCredentials_secretKey :: Lens.Lens' AmplitudeConnectorProfileCredentials Prelude.Text
amplitudeConnectorProfileCredentials_secretKey = Lens.lens (\AmplitudeConnectorProfileCredentials' {secretKey} -> secretKey) (\s@AmplitudeConnectorProfileCredentials' {} a -> s {secretKey = a} :: AmplitudeConnectorProfileCredentials) Prelude.. Core._Sensitive

instance
  Prelude.Hashable
    AmplitudeConnectorProfileCredentials
  where
  hashWithSalt
    salt'
    AmplitudeConnectorProfileCredentials' {..} =
      salt' `Prelude.hashWithSalt` secretKey
        `Prelude.hashWithSalt` apiKey

instance
  Prelude.NFData
    AmplitudeConnectorProfileCredentials
  where
  rnf AmplitudeConnectorProfileCredentials' {..} =
    Prelude.rnf apiKey
      `Prelude.seq` Prelude.rnf secretKey

instance
  Core.ToJSON
    AmplitudeConnectorProfileCredentials
  where
  toJSON AmplitudeConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("apiKey" Core..= apiKey),
            Prelude.Just ("secretKey" Core..= secretKey)
          ]
      )
