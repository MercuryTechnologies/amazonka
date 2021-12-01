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
-- Module      : Amazonka.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the API key authorization parameters to use to update the
-- connection.
--
-- /See:/ 'newUpdateConnectionApiKeyAuthRequestParameters' smart constructor.
data UpdateConnectionApiKeyAuthRequestParameters = UpdateConnectionApiKeyAuthRequestParameters'
  { -- | The value associated with teh API key to use for authorization.
    apiKeyValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the API key to use for authorization.
    apiKeyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConnectionApiKeyAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKeyValue', 'updateConnectionApiKeyAuthRequestParameters_apiKeyValue' - The value associated with teh API key to use for authorization.
--
-- 'apiKeyName', 'updateConnectionApiKeyAuthRequestParameters_apiKeyName' - The name of the API key to use for authorization.
newUpdateConnectionApiKeyAuthRequestParameters ::
  UpdateConnectionApiKeyAuthRequestParameters
newUpdateConnectionApiKeyAuthRequestParameters =
  UpdateConnectionApiKeyAuthRequestParameters'
    { apiKeyValue =
        Prelude.Nothing,
      apiKeyName = Prelude.Nothing
    }

-- | The value associated with teh API key to use for authorization.
updateConnectionApiKeyAuthRequestParameters_apiKeyValue :: Lens.Lens' UpdateConnectionApiKeyAuthRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionApiKeyAuthRequestParameters_apiKeyValue = Lens.lens (\UpdateConnectionApiKeyAuthRequestParameters' {apiKeyValue} -> apiKeyValue) (\s@UpdateConnectionApiKeyAuthRequestParameters' {} a -> s {apiKeyValue = a} :: UpdateConnectionApiKeyAuthRequestParameters)

-- | The name of the API key to use for authorization.
updateConnectionApiKeyAuthRequestParameters_apiKeyName :: Lens.Lens' UpdateConnectionApiKeyAuthRequestParameters (Prelude.Maybe Prelude.Text)
updateConnectionApiKeyAuthRequestParameters_apiKeyName = Lens.lens (\UpdateConnectionApiKeyAuthRequestParameters' {apiKeyName} -> apiKeyName) (\s@UpdateConnectionApiKeyAuthRequestParameters' {} a -> s {apiKeyName = a} :: UpdateConnectionApiKeyAuthRequestParameters)

instance
  Prelude.Hashable
    UpdateConnectionApiKeyAuthRequestParameters
  where
  hashWithSalt
    salt'
    UpdateConnectionApiKeyAuthRequestParameters' {..} =
      salt' `Prelude.hashWithSalt` apiKeyName
        `Prelude.hashWithSalt` apiKeyValue

instance
  Prelude.NFData
    UpdateConnectionApiKeyAuthRequestParameters
  where
  rnf UpdateConnectionApiKeyAuthRequestParameters' {..} =
    Prelude.rnf apiKeyValue
      `Prelude.seq` Prelude.rnf apiKeyName

instance
  Core.ToJSON
    UpdateConnectionApiKeyAuthRequestParameters
  where
  toJSON
    UpdateConnectionApiKeyAuthRequestParameters' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ApiKeyValue" Core..=) Prelude.<$> apiKeyValue,
              ("ApiKeyName" Core..=) Prelude.<$> apiKeyName
            ]
        )
