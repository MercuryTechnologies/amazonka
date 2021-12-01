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
-- Module      : Amazonka.OpsWorks.Types.TemporaryCredential
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.TemporaryCredential where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the data needed by RDP clients such as the Microsoft Remote
-- Desktop Connection to log in to the instance.
--
-- /See:/ 'newTemporaryCredential' smart constructor.
data TemporaryCredential = TemporaryCredential'
  { -- | The instance\'s AWS OpsWorks Stacks ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The user name.
    username :: Prelude.Maybe Prelude.Text,
    -- | The password.
    password :: Prelude.Maybe Prelude.Text,
    -- | The length of time (in minutes) that the grant is valid. When the grant
    -- expires, at the end of this period, the user will no longer be able to
    -- use the credentials to log in. If they are logged in at the time, they
    -- will be automatically logged out.
    validForInMinutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemporaryCredential' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'temporaryCredential_instanceId' - The instance\'s AWS OpsWorks Stacks ID.
--
-- 'username', 'temporaryCredential_username' - The user name.
--
-- 'password', 'temporaryCredential_password' - The password.
--
-- 'validForInMinutes', 'temporaryCredential_validForInMinutes' - The length of time (in minutes) that the grant is valid. When the grant
-- expires, at the end of this period, the user will no longer be able to
-- use the credentials to log in. If they are logged in at the time, they
-- will be automatically logged out.
newTemporaryCredential ::
  TemporaryCredential
newTemporaryCredential =
  TemporaryCredential'
    { instanceId = Prelude.Nothing,
      username = Prelude.Nothing,
      password = Prelude.Nothing,
      validForInMinutes = Prelude.Nothing
    }

-- | The instance\'s AWS OpsWorks Stacks ID.
temporaryCredential_instanceId :: Lens.Lens' TemporaryCredential (Prelude.Maybe Prelude.Text)
temporaryCredential_instanceId = Lens.lens (\TemporaryCredential' {instanceId} -> instanceId) (\s@TemporaryCredential' {} a -> s {instanceId = a} :: TemporaryCredential)

-- | The user name.
temporaryCredential_username :: Lens.Lens' TemporaryCredential (Prelude.Maybe Prelude.Text)
temporaryCredential_username = Lens.lens (\TemporaryCredential' {username} -> username) (\s@TemporaryCredential' {} a -> s {username = a} :: TemporaryCredential)

-- | The password.
temporaryCredential_password :: Lens.Lens' TemporaryCredential (Prelude.Maybe Prelude.Text)
temporaryCredential_password = Lens.lens (\TemporaryCredential' {password} -> password) (\s@TemporaryCredential' {} a -> s {password = a} :: TemporaryCredential)

-- | The length of time (in minutes) that the grant is valid. When the grant
-- expires, at the end of this period, the user will no longer be able to
-- use the credentials to log in. If they are logged in at the time, they
-- will be automatically logged out.
temporaryCredential_validForInMinutes :: Lens.Lens' TemporaryCredential (Prelude.Maybe Prelude.Int)
temporaryCredential_validForInMinutes = Lens.lens (\TemporaryCredential' {validForInMinutes} -> validForInMinutes) (\s@TemporaryCredential' {} a -> s {validForInMinutes = a} :: TemporaryCredential)

instance Core.FromJSON TemporaryCredential where
  parseJSON =
    Core.withObject
      "TemporaryCredential"
      ( \x ->
          TemporaryCredential'
            Prelude.<$> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "Password")
            Prelude.<*> (x Core..:? "ValidForInMinutes")
      )

instance Prelude.Hashable TemporaryCredential where
  hashWithSalt salt' TemporaryCredential' {..} =
    salt' `Prelude.hashWithSalt` validForInMinutes
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData TemporaryCredential where
  rnf TemporaryCredential' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf validForInMinutes
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf username
