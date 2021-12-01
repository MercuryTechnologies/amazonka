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
-- Module      : Amazonka.AppFlow.Types.InforNexusConnectorProfileCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.InforNexusConnectorProfileCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required by Infor Nexus.
--
-- /See:/ 'newInforNexusConnectorProfileCredentials' smart constructor.
data InforNexusConnectorProfileCredentials = InforNexusConnectorProfileCredentials'
  { -- | The Access Key portion of the credentials.
    accessKeyId :: Core.Sensitive Prelude.Text,
    -- | The identifier for the user.
    userId :: Prelude.Text,
    -- | The secret key used to sign requests.
    secretAccessKey :: Prelude.Text,
    -- | The encryption keys used to encrypt data.
    datakey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InforNexusConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'inforNexusConnectorProfileCredentials_accessKeyId' - The Access Key portion of the credentials.
--
-- 'userId', 'inforNexusConnectorProfileCredentials_userId' - The identifier for the user.
--
-- 'secretAccessKey', 'inforNexusConnectorProfileCredentials_secretAccessKey' - The secret key used to sign requests.
--
-- 'datakey', 'inforNexusConnectorProfileCredentials_datakey' - The encryption keys used to encrypt data.
newInforNexusConnectorProfileCredentials ::
  -- | 'accessKeyId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'secretAccessKey'
  Prelude.Text ->
  -- | 'datakey'
  Prelude.Text ->
  InforNexusConnectorProfileCredentials
newInforNexusConnectorProfileCredentials
  pAccessKeyId_
  pUserId_
  pSecretAccessKey_
  pDatakey_ =
    InforNexusConnectorProfileCredentials'
      { accessKeyId =
          Core._Sensitive
            Lens.# pAccessKeyId_,
        userId = pUserId_,
        secretAccessKey = pSecretAccessKey_,
        datakey = pDatakey_
      }

-- | The Access Key portion of the credentials.
inforNexusConnectorProfileCredentials_accessKeyId :: Lens.Lens' InforNexusConnectorProfileCredentials Prelude.Text
inforNexusConnectorProfileCredentials_accessKeyId = Lens.lens (\InforNexusConnectorProfileCredentials' {accessKeyId} -> accessKeyId) (\s@InforNexusConnectorProfileCredentials' {} a -> s {accessKeyId = a} :: InforNexusConnectorProfileCredentials) Prelude.. Core._Sensitive

-- | The identifier for the user.
inforNexusConnectorProfileCredentials_userId :: Lens.Lens' InforNexusConnectorProfileCredentials Prelude.Text
inforNexusConnectorProfileCredentials_userId = Lens.lens (\InforNexusConnectorProfileCredentials' {userId} -> userId) (\s@InforNexusConnectorProfileCredentials' {} a -> s {userId = a} :: InforNexusConnectorProfileCredentials)

-- | The secret key used to sign requests.
inforNexusConnectorProfileCredentials_secretAccessKey :: Lens.Lens' InforNexusConnectorProfileCredentials Prelude.Text
inforNexusConnectorProfileCredentials_secretAccessKey = Lens.lens (\InforNexusConnectorProfileCredentials' {secretAccessKey} -> secretAccessKey) (\s@InforNexusConnectorProfileCredentials' {} a -> s {secretAccessKey = a} :: InforNexusConnectorProfileCredentials)

-- | The encryption keys used to encrypt data.
inforNexusConnectorProfileCredentials_datakey :: Lens.Lens' InforNexusConnectorProfileCredentials Prelude.Text
inforNexusConnectorProfileCredentials_datakey = Lens.lens (\InforNexusConnectorProfileCredentials' {datakey} -> datakey) (\s@InforNexusConnectorProfileCredentials' {} a -> s {datakey = a} :: InforNexusConnectorProfileCredentials)

instance
  Prelude.Hashable
    InforNexusConnectorProfileCredentials
  where
  hashWithSalt
    salt'
    InforNexusConnectorProfileCredentials' {..} =
      salt' `Prelude.hashWithSalt` datakey
        `Prelude.hashWithSalt` secretAccessKey
        `Prelude.hashWithSalt` userId
        `Prelude.hashWithSalt` accessKeyId

instance
  Prelude.NFData
    InforNexusConnectorProfileCredentials
  where
  rnf InforNexusConnectorProfileCredentials' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf datakey
      `Prelude.seq` Prelude.rnf secretAccessKey
      `Prelude.seq` Prelude.rnf userId

instance
  Core.ToJSON
    InforNexusConnectorProfileCredentials
  where
  toJSON InforNexusConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("accessKeyId" Core..= accessKeyId),
            Prelude.Just ("userId" Core..= userId),
            Prelude.Just
              ("secretAccessKey" Core..= secretAccessKey),
            Prelude.Just ("datakey" Core..= datakey)
          ]
      )
