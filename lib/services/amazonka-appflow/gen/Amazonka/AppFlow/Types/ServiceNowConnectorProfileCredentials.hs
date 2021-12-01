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
-- Module      : Amazonka.AppFlow.Types.ServiceNowConnectorProfileCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ServiceNowConnectorProfileCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials required when using
-- ServiceNow.
--
-- /See:/ 'newServiceNowConnectorProfileCredentials' smart constructor.
data ServiceNowConnectorProfileCredentials = ServiceNowConnectorProfileCredentials'
  { -- | The name of the user.
    username :: Prelude.Text,
    -- | The password that corresponds to the user name.
    password :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceNowConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'username', 'serviceNowConnectorProfileCredentials_username' - The name of the user.
--
-- 'password', 'serviceNowConnectorProfileCredentials_password' - The password that corresponds to the user name.
newServiceNowConnectorProfileCredentials ::
  -- | 'username'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  ServiceNowConnectorProfileCredentials
newServiceNowConnectorProfileCredentials
  pUsername_
  pPassword_ =
    ServiceNowConnectorProfileCredentials'
      { username =
          pUsername_,
        password =
          Core._Sensitive Lens.# pPassword_
      }

-- | The name of the user.
serviceNowConnectorProfileCredentials_username :: Lens.Lens' ServiceNowConnectorProfileCredentials Prelude.Text
serviceNowConnectorProfileCredentials_username = Lens.lens (\ServiceNowConnectorProfileCredentials' {username} -> username) (\s@ServiceNowConnectorProfileCredentials' {} a -> s {username = a} :: ServiceNowConnectorProfileCredentials)

-- | The password that corresponds to the user name.
serviceNowConnectorProfileCredentials_password :: Lens.Lens' ServiceNowConnectorProfileCredentials Prelude.Text
serviceNowConnectorProfileCredentials_password = Lens.lens (\ServiceNowConnectorProfileCredentials' {password} -> password) (\s@ServiceNowConnectorProfileCredentials' {} a -> s {password = a} :: ServiceNowConnectorProfileCredentials) Prelude.. Core._Sensitive

instance
  Prelude.Hashable
    ServiceNowConnectorProfileCredentials
  where
  hashWithSalt
    salt'
    ServiceNowConnectorProfileCredentials' {..} =
      salt' `Prelude.hashWithSalt` password
        `Prelude.hashWithSalt` username

instance
  Prelude.NFData
    ServiceNowConnectorProfileCredentials
  where
  rnf ServiceNowConnectorProfileCredentials' {..} =
    Prelude.rnf username
      `Prelude.seq` Prelude.rnf password

instance
  Core.ToJSON
    ServiceNowConnectorProfileCredentials
  where
  toJSON ServiceNowConnectorProfileCredentials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("username" Core..= username),
            Prelude.Just ("password" Core..= password)
          ]
      )
