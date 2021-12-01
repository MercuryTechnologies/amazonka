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
-- Module      : Amazonka.AppSync.Types.ApiKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.ApiKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an API key.
--
-- Customers invoke AppSync GraphQL API operations with API keys as an
-- identity mechanism. There are two key versions:
--
-- __da1__: This version was introduced at launch in November 2017. These
-- keys always expire after 7 days. Key expiration is managed by Amazon
-- DynamoDB TTL. The keys ceased to be valid after February 21, 2018 and
-- should not be used after that date.
--
-- -   @ListApiKeys@ returns the expiration time in milliseconds.
--
-- -   @CreateApiKey@ returns the expiration time in milliseconds.
--
-- -   @UpdateApiKey@ is not available for this key version.
--
-- -   @DeleteApiKey@ deletes the item from the table.
--
-- -   Expiration is stored in Amazon DynamoDB as milliseconds. This
--     results in a bug where keys are not automatically deleted because
--     DynamoDB expects the TTL to be stored in seconds. As a one-time
--     action, we will delete these keys from the table after February 21,
--     2018.
--
-- __da2__: This version was introduced in February 2018 when AppSync added
-- support to extend key expiration.
--
-- -   @ListApiKeys@ returns the expiration time and deletion time in
--     seconds.
--
-- -   @CreateApiKey@ returns the expiration time and deletion time in
--     seconds and accepts a user-provided expiration time in seconds.
--
-- -   @UpdateApiKey@ returns the expiration time and and deletion time in
--     seconds and accepts a user-provided expiration time in seconds.
--     Expired API keys are kept for 60 days after the expiration time. Key
--     expiration time can be updated while the key is not deleted.
--
-- -   @DeleteApiKey@ deletes the item from the table.
--
-- -   Expiration is stored in Amazon DynamoDB as seconds. After the
--     expiration time, using the key to authenticate will fail. But the
--     key can be reinstated before deletion.
--
-- -   Deletion is stored in Amazon DynamoDB as seconds. The key will be
--     deleted after deletion time.
--
-- /See:/ 'newApiKey' smart constructor.
data ApiKey = ApiKey'
  { -- | The time after which the API key expires. The date is represented as
    -- seconds since the epoch, rounded down to the nearest hour.
    expires :: Prelude.Maybe Prelude.Integer,
    -- | The time after which the API key is deleted. The date is represented as
    -- seconds since the epoch, rounded down to the nearest hour.
    deletes :: Prelude.Maybe Prelude.Integer,
    -- | The API key ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | A description of the purpose of the API key.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expires', 'apiKey_expires' - The time after which the API key expires. The date is represented as
-- seconds since the epoch, rounded down to the nearest hour.
--
-- 'deletes', 'apiKey_deletes' - The time after which the API key is deleted. The date is represented as
-- seconds since the epoch, rounded down to the nearest hour.
--
-- 'id', 'apiKey_id' - The API key ID.
--
-- 'description', 'apiKey_description' - A description of the purpose of the API key.
newApiKey ::
  ApiKey
newApiKey =
  ApiKey'
    { expires = Prelude.Nothing,
      deletes = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The time after which the API key expires. The date is represented as
-- seconds since the epoch, rounded down to the nearest hour.
apiKey_expires :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Integer)
apiKey_expires = Lens.lens (\ApiKey' {expires} -> expires) (\s@ApiKey' {} a -> s {expires = a} :: ApiKey)

-- | The time after which the API key is deleted. The date is represented as
-- seconds since the epoch, rounded down to the nearest hour.
apiKey_deletes :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Integer)
apiKey_deletes = Lens.lens (\ApiKey' {deletes} -> deletes) (\s@ApiKey' {} a -> s {deletes = a} :: ApiKey)

-- | The API key ID.
apiKey_id :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Text)
apiKey_id = Lens.lens (\ApiKey' {id} -> id) (\s@ApiKey' {} a -> s {id = a} :: ApiKey)

-- | A description of the purpose of the API key.
apiKey_description :: Lens.Lens' ApiKey (Prelude.Maybe Prelude.Text)
apiKey_description = Lens.lens (\ApiKey' {description} -> description) (\s@ApiKey' {} a -> s {description = a} :: ApiKey)

instance Core.FromJSON ApiKey where
  parseJSON =
    Core.withObject
      "ApiKey"
      ( \x ->
          ApiKey'
            Prelude.<$> (x Core..:? "expires")
            Prelude.<*> (x Core..:? "deletes")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable ApiKey where
  hashWithSalt salt' ApiKey' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` deletes
      `Prelude.hashWithSalt` expires

instance Prelude.NFData ApiKey where
  rnf ApiKey' {..} =
    Prelude.rnf expires
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf deletes
