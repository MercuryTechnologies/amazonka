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
-- Module      : Amazonka.Nimble.DeleteStudioComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a studio component resource.
module Amazonka.Nimble.DeleteStudioComponent
  ( -- * Creating a Request
    DeleteStudioComponent (..),
    newDeleteStudioComponent,

    -- * Request Lenses
    deleteStudioComponent_clientToken,
    deleteStudioComponent_studioId,
    deleteStudioComponent_studioComponentId,

    -- * Destructuring the Response
    DeleteStudioComponentResponse (..),
    newDeleteStudioComponentResponse,

    -- * Response Lenses
    deleteStudioComponentResponse_studioComponent,
    deleteStudioComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStudioComponent' smart constructor.
data DeleteStudioComponent = DeleteStudioComponent'
  { -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The studio component ID.
    studioComponentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteStudioComponent_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'studioId', 'deleteStudioComponent_studioId' - The studio ID.
--
-- 'studioComponentId', 'deleteStudioComponent_studioComponentId' - The studio component ID.
newDeleteStudioComponent ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'studioComponentId'
  Prelude.Text ->
  DeleteStudioComponent
newDeleteStudioComponent
  pStudioId_
  pStudioComponentId_ =
    DeleteStudioComponent'
      { clientToken =
          Prelude.Nothing,
        studioId = pStudioId_,
        studioComponentId = pStudioComponentId_
      }

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
deleteStudioComponent_clientToken :: Lens.Lens' DeleteStudioComponent (Prelude.Maybe Prelude.Text)
deleteStudioComponent_clientToken = Lens.lens (\DeleteStudioComponent' {clientToken} -> clientToken) (\s@DeleteStudioComponent' {} a -> s {clientToken = a} :: DeleteStudioComponent)

-- | The studio ID.
deleteStudioComponent_studioId :: Lens.Lens' DeleteStudioComponent Prelude.Text
deleteStudioComponent_studioId = Lens.lens (\DeleteStudioComponent' {studioId} -> studioId) (\s@DeleteStudioComponent' {} a -> s {studioId = a} :: DeleteStudioComponent)

-- | The studio component ID.
deleteStudioComponent_studioComponentId :: Lens.Lens' DeleteStudioComponent Prelude.Text
deleteStudioComponent_studioComponentId = Lens.lens (\DeleteStudioComponent' {studioComponentId} -> studioComponentId) (\s@DeleteStudioComponent' {} a -> s {studioComponentId = a} :: DeleteStudioComponent)

instance Core.AWSRequest DeleteStudioComponent where
  type
    AWSResponse DeleteStudioComponent =
      DeleteStudioComponentResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteStudioComponentResponse'
            Prelude.<$> (x Core..?> "studioComponent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStudioComponent where
  hashWithSalt salt' DeleteStudioComponent' {..} =
    salt' `Prelude.hashWithSalt` studioComponentId
      `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData DeleteStudioComponent where
  rnf DeleteStudioComponent' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf studioComponentId
      `Prelude.seq` Prelude.rnf studioId

instance Core.ToHeaders DeleteStudioComponent where
  toHeaders DeleteStudioComponent' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DeleteStudioComponent where
  toPath DeleteStudioComponent' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/studio-components/",
        Core.toBS studioComponentId
      ]

instance Core.ToQuery DeleteStudioComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStudioComponentResponse' smart constructor.
data DeleteStudioComponentResponse = DeleteStudioComponentResponse'
  { -- | Information about the studio component.
    studioComponent :: Prelude.Maybe StudioComponent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioComponent', 'deleteStudioComponentResponse_studioComponent' - Information about the studio component.
--
-- 'httpStatus', 'deleteStudioComponentResponse_httpStatus' - The response's http status code.
newDeleteStudioComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStudioComponentResponse
newDeleteStudioComponentResponse pHttpStatus_ =
  DeleteStudioComponentResponse'
    { studioComponent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the studio component.
deleteStudioComponentResponse_studioComponent :: Lens.Lens' DeleteStudioComponentResponse (Prelude.Maybe StudioComponent)
deleteStudioComponentResponse_studioComponent = Lens.lens (\DeleteStudioComponentResponse' {studioComponent} -> studioComponent) (\s@DeleteStudioComponentResponse' {} a -> s {studioComponent = a} :: DeleteStudioComponentResponse)

-- | The response's http status code.
deleteStudioComponentResponse_httpStatus :: Lens.Lens' DeleteStudioComponentResponse Prelude.Int
deleteStudioComponentResponse_httpStatus = Lens.lens (\DeleteStudioComponentResponse' {httpStatus} -> httpStatus) (\s@DeleteStudioComponentResponse' {} a -> s {httpStatus = a} :: DeleteStudioComponentResponse)

instance Prelude.NFData DeleteStudioComponentResponse where
  rnf DeleteStudioComponentResponse' {..} =
    Prelude.rnf studioComponent
      `Prelude.seq` Prelude.rnf httpStatus
