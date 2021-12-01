{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecretsManager.DeleteResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource-based permission policy attached to the secret.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:DeleteResourcePolicy
--
-- __Related operations__
--
-- -   To attach a resource policy to a secret, use PutResourcePolicy.
--
-- -   To retrieve the current resource-based policy attached to a secret,
--     use GetResourcePolicy.
--
-- -   To list all of the currently available secrets, use ListSecrets.
module Amazonka.SecretsManager.DeleteResourcePolicy
  ( -- * Creating a Request
    DeleteResourcePolicy (..),
    newDeleteResourcePolicy,

    -- * Request Lenses
    deleteResourcePolicy_secretId,

    -- * Destructuring the Response
    DeleteResourcePolicyResponse (..),
    newDeleteResourcePolicyResponse,

    -- * Response Lenses
    deleteResourcePolicyResponse_arn,
    deleteResourcePolicyResponse_name,
    deleteResourcePolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newDeleteResourcePolicy' smart constructor.
data DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | Specifies the secret that you want to delete the attached resource-based
    -- policy for. You can specify either the Amazon Resource Name (ARN) or the
    -- friendly name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'deleteResourcePolicy_secretId' - Specifies the secret that you want to delete the attached resource-based
-- policy for. You can specify either the Amazon Resource Name (ARN) or the
-- friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
newDeleteResourcePolicy ::
  -- | 'secretId'
  Prelude.Text ->
  DeleteResourcePolicy
newDeleteResourcePolicy pSecretId_ =
  DeleteResourcePolicy' {secretId = pSecretId_}

-- | Specifies the secret that you want to delete the attached resource-based
-- policy for. You can specify either the Amazon Resource Name (ARN) or the
-- friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
deleteResourcePolicy_secretId :: Lens.Lens' DeleteResourcePolicy Prelude.Text
deleteResourcePolicy_secretId = Lens.lens (\DeleteResourcePolicy' {secretId} -> secretId) (\s@DeleteResourcePolicy' {} a -> s {secretId = a} :: DeleteResourcePolicy)

instance Core.AWSRequest DeleteResourcePolicy where
  type
    AWSResponse DeleteResourcePolicy =
      DeleteResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Prelude.<$> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResourcePolicy where
  hashWithSalt salt' DeleteResourcePolicy' {..} =
    salt' `Prelude.hashWithSalt` secretId

instance Prelude.NFData DeleteResourcePolicy where
  rnf DeleteResourcePolicy' {..} = Prelude.rnf secretId

instance Core.ToHeaders DeleteResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.DeleteResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Core..= secretId)]
      )

instance Core.ToPath DeleteResourcePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { -- | The ARN of the secret that the resource-based policy was deleted for.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret that the resource-based policy was
    -- deleted for.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteResourcePolicyResponse_arn' - The ARN of the secret that the resource-based policy was deleted for.
--
-- 'name', 'deleteResourcePolicyResponse_name' - The friendly name of the secret that the resource-based policy was
-- deleted for.
--
-- 'httpStatus', 'deleteResourcePolicyResponse_httpStatus' - The response's http status code.
newDeleteResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteResourcePolicyResponse
newDeleteResourcePolicyResponse pHttpStatus_ =
  DeleteResourcePolicyResponse'
    { arn =
        Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret that the resource-based policy was deleted for.
deleteResourcePolicyResponse_arn :: Lens.Lens' DeleteResourcePolicyResponse (Prelude.Maybe Prelude.Text)
deleteResourcePolicyResponse_arn = Lens.lens (\DeleteResourcePolicyResponse' {arn} -> arn) (\s@DeleteResourcePolicyResponse' {} a -> s {arn = a} :: DeleteResourcePolicyResponse)

-- | The friendly name of the secret that the resource-based policy was
-- deleted for.
deleteResourcePolicyResponse_name :: Lens.Lens' DeleteResourcePolicyResponse (Prelude.Maybe Prelude.Text)
deleteResourcePolicyResponse_name = Lens.lens (\DeleteResourcePolicyResponse' {name} -> name) (\s@DeleteResourcePolicyResponse' {} a -> s {name = a} :: DeleteResourcePolicyResponse)

-- | The response's http status code.
deleteResourcePolicyResponse_httpStatus :: Lens.Lens' DeleteResourcePolicyResponse Prelude.Int
deleteResourcePolicyResponse_httpStatus = Lens.lens (\DeleteResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcePolicyResponse' {} a -> s {httpStatus = a} :: DeleteResourcePolicyResponse)

instance Prelude.NFData DeleteResourcePolicyResponse where
  rnf DeleteResourcePolicyResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
